> module Test.Rufous.Extract where

> import Control.Lens ((&), (^.), (%~))

DUG Extraction is non-trivial, and there are a few problems to consider:
    - Automation
        + To reduce strain on the programmer, as few changes should be required to extract a DUG from an existing program.
    - Laziness 
        + the extracter must not force evalation of the arguments any more than the program would

The approach taken is to utilize the class/instance hierarchy that defines the signature,
and to define a _wrapper_ instance like so:

class ListADT l where
    cons :: a -> l a -> l a
    nil  :: l a

then say we had an instance:

instance ListADT [] where
    cons = (:)
    nil  = []

Then we could generate a wrapper instance like so:
(i.e. using TH)

makeRufousExtractor ''ListADT
====>

data WrappedADT t x = WrappedADT Int [Int] (t x)
instance ListADT t => ListADT (WrappedADT t) where
    cons x (WrappedADT i _ xs) = WrappedADT _generateFreshId [i] (cons x xs)
    nil = WrappedADT _generateFreshId [] nil

Then, the code only has be to re-written to use the implementation-ambiguous interface. Which should be simple.

> import System.IO.Unsafe
> import Unsafe.Coerce
> import Data.IORef
> import System.Random

Actually generating the fresh IDs and logging the versions is tricky to do in a type-safe way, and forcing the end-user to wrap 
everything in IO is not an option. Instead we generate fresh names and perform the logging operations in an unsafe block
that pretends it's pure -- this also caches the value of the fresh id for us.

> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.DUG as D
> import qualified Data.Map as M

> import Data.Either

A wrapped node stores the current node id and the list of nodes used to construct it 
(the incoming edges in the DUG)

> type ExtractArg t x = S.Arg (WrappedADT t x) x
> type ExtractedDUG = D.DUG ()
> data WrappedADT t x =
>   WrappedADT 
>       { nodeId :: Int 
>       , nodeArgs :: [ExtractArg t x]
>       , nodeOp :: String
>       , value :: Either x (t x)  -- this is either an observeration node or an operation node
>       }
>   deriving (Show)

> getVersion :: WrappedADT t x -> t x
> getVersion w = case value w of
>   Right x -> x
>   Left _  -> error "getVersion :: expected Right"

> idCounter :: IORef Int
> idCounter = unsafePerformIO $ newIORef (0 :: Int)
> {-# NOINLINE idCounter #-}

> freshId :: IO Int
> freshId = do
>   x <- readIORef idCounter
>   writeIORef idCounter (x + 1)
>   return x

During extraction, some global state must be maintained

> data ExtractorState t x = 
>   ExtractorState 
>       { nodes :: M.Map Int (WrappedADT t x)
>       }

> emptyExtractorState :: ExtractorState t x 
> emptyExtractorState = ExtractorState M.empty

> state :: IORef (ExtractorState t x)
> state = unsafePerformIO $ newIORef emptyExtractorState
> {-# NOINLINE state #-}

Then a set of logging functions can update the state in an unsafe way:

> addToState :: WrappedADT t x -> IO ()
> addToState (w@(WrappedADT i _ _ _)) = do
>   st <- readIORef state
>   let st' = ExtractorState (M.insert i w (nodes st))
>   writeIORef state st'

> _log_operation :: String -> [ExtractArg t x] -> t x -> WrappedADT t x
> _log_operation name args v = unsafePerformIO $ do
>   _id <- freshId
>   let wrapper = WrappedADT _id args name (Right v)
>   addToState wrapper
>   return wrapper
> {-# NOINLINE _log_operation #-}
>   
> _log_observer :: String -> [ExtractArg t x] -> x -> x
> _log_observer name args v = unsafePerformIO $ do
>   _id <- freshId
>   let wrapper = WrappedADT _id args name (Left v)
>   addToState wrapper
>   return v
> {-# NOINLINE _log_observer #-}

> extractDUG :: S.Signature -> ExtractorState t x -> ExtractedDUG
> extractDUG s state = insertNodes (M.elems $ nodes $ state) (D.emptyDug)
>   where
>       insertNodes :: [WrappedADT t x] -> ExtractedDUG -> ExtractedDUG
>       insertNodes [] d = d
>       insertNodes (w:ws) d = insertNodes ws (insertNode w d)
>       insertNode :: WrappedADT t x -> ExtractedDUG -> ExtractedDUG
>       insertNode w d = d & D.insertOp (D.Node (getOp w) (getArgs w) (nodeId w) ())
>       getOp w = (s ^. S.operations) M.! (nodeOp w)
>       getArgs w = map getArg (nodeArgs w)
>       getArg (S.Version w) = S.Version (nodeId w)
>       getArg (S.NonVersion a) = S.NonVersion (unsafeCoerce a :: Int)  -- TODO: better handling of non-version args

> init_state :: IO ()
> init_state = do
>   writeIORef state emptyExtractorState

> read_state :: S.Signature -> IO (ExtractedDUG)
> read_state s = do
>   extractor_state <- readIORef state
>   -- todo: read the state and build the DUG
>   let dug = extractDUG s extractor_state
>   print (unsafeCoerce (nodes extractor_state) :: M.Map Int (WrappedADT [] Int))
>   putStrLn $ D.pprintDUG dug
>   return dug

> extract :: S.Signature -> IO a -> IO (a, ExtractedDUG)
> extract s a = do
>   init_state
>   v <- a
>   d <- read_state s
>   return (v, d)
