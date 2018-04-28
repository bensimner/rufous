> module Test.Rufous.Extract where

> import Lens.Micro ((&), (^.), (%~))
> import Data.List (sortOn)

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
> import Control.Concurrent.MVar
> import System.Random

Actually generating the fresh IDs and logging the versions is tricky to do in a type-safe way, and forcing the end-user to wrap 
everything in IO is not an option. Instead we generate fresh names and perform the logging operations in an unsafe block
that pretends it's pure -- this also caches the value of the fresh id for us.

> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.DUG as D
> import qualified Test.Rufous.Internal.Timing as T
> import qualified Data.Map as M

> import Data.Either

A wrapped node stores the current node id and the list of nodes used to construct it 
(the incoming edges in the DUG)

> type ExtractArg t x = S.Arg (WrappedADT t x) x Int Bool
> type UnwrappedArg t x = S.Arg (t x) x Int Bool
> type ExtractedDUG = D.DUG ()
> data WrappedADT t x =
>   WrappedADT 
>       { nodeId :: Int 
>       , nodeArgs :: [ExtractArg t x]
>       , nodeOp :: String
>       , value :: UnwrappedArg t x
>       }
>   deriving (Show)

> getVersion :: WrappedADT t x -> t x
> getVersion w = case value w of
>   S.Version x -> x
>   _  -> error "getVersion :: expected Version"

During extraction, some global state must be maintained

> data ExtractorState t x = 
>   ExtractorState 
>       { nodes :: M.Map Int (WrappedADT t x)
>       , currentIndex :: Int
>       }

> emptyExtractorState :: ExtractorState t x 
> emptyExtractorState = ExtractorState M.empty 0

> state :: MVar (ExtractorState t x)
> state = unsafePerformIO $ newEmptyMVar
> {-# NOINLINE state #-}

Then a set of logging functions can update the state in an unsafe way:

> updateWrapper :: String -> [ExtractArg t x] -> UnwrappedArg t x -> IO (WrappedADT t x)
> updateWrapper name args v = do
>   (ExtractorState m i) <- takeMVar state
>   let w = WrappedADT i args name v
>   let st' = ExtractorState (M.insert i w m) (i + 1)
>   putMVar state st'
>   return w

> _log_operation :: String -> [ExtractArg t x] -> t x -> WrappedADT t x
> _log_operation name args v = unsafePerformIO $ updateWrapper name args (S.Version v)
> {-# NOINLINE _log_operation #-}
>   
> _log_observer :: String -> [ExtractArg t x] -> x -> x
> _log_observer name args v = unsafePerformIO $ do
>   updateWrapper name args (S.NonVersion (S.VersionParam v))
>   return v
> {-# NOINLINE _log_observer #-}

> _log_observer_nv :: String -> [ExtractArg t x] -> S.NVA x Int Bool -> c -> c
> _log_observer_nv name args nva v = unsafePerformIO $ do
>   updateWrapper name args (S.NonVersion nva)
>   return v
> {-# NOINLINE _log_observer_nv #-}

> extractDUG :: S.Signature -> ExtractorState t x -> ExtractedDUG
> extractDUG s state = insertNodes (sortOn nodeId (M.elems . nodes $ state)) D.emptyDug
>   where
>       insertNodes :: [WrappedADT t x] -> ExtractedDUG -> ExtractedDUG
>       insertNodes [] d = d
>       insertNodes (w:ws) d = insertNodes ws (insertNode w d)
>       insertNode :: WrappedADT t x -> ExtractedDUG -> ExtractedDUG
>       insertNode w d = d & D.insertOp (D.Node (getOp w) (getArgs w) (nodeId w) ())
>       getOp w = (s ^. S.operations) M.! (nodeOp w)
>       getArgs w = map getArg (nodeArgs w)
>       getArg :: ExtractArg t x -> S.Arg Int Int Int Bool
>       getArg (S.Version w) = S.Version (nodeId w)
>       getArg (S.NonVersion (S.VersionParam a)) = S.NonVersion (S.VersionParam (unsafeCoerce a :: Int))  -- force instantiation to Int
>       getArg (S.NonVersion (S.IntArg a)) = S.NonVersion (S.IntArg a)
>       getArg (S.NonVersion (S.BoolArg a)) = S.NonVersion (S.BoolArg a)

> init_state :: IO ()
> init_state = do
>   putMVar state emptyExtractorState

> read_state :: S.Signature -> IO (ExtractedDUG)
> read_state s = do
>   extractor_state <- takeMVar state
>   let dug = extractDUG s extractor_state
>   return dug

> extract :: S.Signature -> IO a -> IO (a, ExtractedDUG)
> extract s a = T.time "EXTRACT PHASE" $ do
>   init_state
>   v <- a
>   d <- read_state s
>   return (v, d)
