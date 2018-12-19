{-# LANGUAGE RankNTypes #-}
module Test.Rufous.Internal.Generate.Core where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Lens
import Debug.Trace

import Data.Dynamic
import System.IO.Unsafe

import qualified Data.Map as M
import qualified Data.Set as St

import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Run as R
import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Profile as P

import Test.Rufous.Internal.Generate.Types
import Test.Rufous.Internal.Generate.Buffer
import qualified Test.Rufous.Internal.Generate.Random as R
import qualified Test.Rufous.Internal.Generate.MSet as MSt

-- | Build will run the inflate/deflate cycle calling inflate 'size' times.
build :: Int -> GenState D.DUG
build 0 = use dug
build size = do
   inflate
   tryDeflate
   d <- use dug
   () <- unsafePerformIO $ do
            print $ "Step " ++ show size
            D.printDUG ("test_dug_" ++ pad size) d
            return (return ())
   build (size - 1)

pad :: Int -> String
pad i = replicate (3 - length unpad) '0' ++ unpad
   where unpad = show i

-- | Inflation is where we add new buffered operations to the 
-- state's buffer.  Depending on the Profile.
inflate :: GenState ()
inflate = genBufferedOp >>= pushBuffer

genBufferedOp :: GenState BufferedOperation
genBufferedOp = do
   st <- get
   op <- R.genAccordingTo (st^.profile^.P.operationWeights) (st ^. sig ^. S.operations)
   let opName = op^.S.opName
   let Just op = st ^. sig . S.operations . at opName
   args <- sequence $ genAbstractArgs op
   return $ BufferedOperation op args 2000

genAbstractArgs :: S.Operation -> [GenState BufferedArg]
genAbstractArgs op = genAbstractFromArgTy (op^.S.opName) <$> op^.S.opArgTypes

genAbstractFromArgTy :: String -> S.ArgType -> GenState BufferedArg
genAbstractFromArgTy opName ty = do
   prof <- use profile
   pers <- R.genBool ((prof^.P.persistentApplicationWeights) M.! opName)
   return $ Abstract ty (if pers then Persistent else Ephemeral)

tryDeflate :: GenState ()
tryDeflate = do
   bops <- popBufferAll
   bs <- sequence $ map tryDeflateBop bops
   if or bs then do -- if we successfully created any new versions
      tryDeflate    -- run again three times, once to try fill NVAs, once to fill VAs and once to try commit anything new
      tryDeflate
      tryDeflate
   else
      return ()

-- | Try to "deflate"  (i.e. fill and commit) a buffered operation.
-- if success then add new node directly to DUG, and return True
-- otherwise it places it back on the buffer and returns False.
tryDeflateBop bop | satisifed bop          = doDeflate True (commitBop bop)
tryDeflateBop bop | partiallySatisifed bop = doDeflate False (tryFillNVAs bop)
tryDeflateBop bop = doDeflate False (trySatisfyAndPush bop)
doDeflate b f = f >> return b


-- | "commit" a buffered operation; adding it to the partially-built DUG.
commitBop :: BufferedOperation -> GenState ()
commitBop bop = do
      result <- runShadow bop
      case result of
         R.RunSuccess _ -> cont
         R.RunExcept R.NotImplemented -> cont
         R.RunTypeMismatch -> error "Shadow type mismatch"
         R.RunExcept R.GuardFailed ->
            case bop^.life of
               0 -> return ()
               i -> abstractify bop{_life=i-1} >>= pushBuffer
   where cont = do
         d <- use dug
         let nodeId = D.nextId d
         Just shadow <- makeShadow bop
         dug %= D.pushNew (bop^.bufOp) (dugArgs bop) shadow
         prof <- use profile
         alive <- R.genBool (1 - prof^.P.mortality)
         if bop^.bufOp^.S.opCategory /= S.Observer && alive then
            living %= St.insert nodeId
         else return ()
         mutators  . infants %= MSt.insert nodeId
         observers . infants %= MSt.insert nodeId

abstractify :: BufferedOperation -> GenState BufferedOperation
abstractify bop = do
   args <- sequence $ genAbstractArgs (bop^.bufOp)
   return bop{_bufArgs=args}

dugArgs :: BufferedOperation -> [D.DUGArg]
dugArgs bop = [a | Concrete a <- bop^.bufArgs]

makeShadow :: BufferedOperation -> GenState (Maybe Dynamic)
makeShadow bop = do
   d <- use dug
   sig <- use sig
   let Just shadow = sig ^. S.shadowImpl
   return $ Just $ R.makeDynCell shadow d (bop^.bufOp) (dugArgs bop)

runShadow :: BufferedOperation -> GenState (R.RunResult)
runShadow bop = do
   sig <- use sig
   let name = bop^.bufOp^.S.opName
   let Just shadowImpl = sig ^. S.shadowImpl
   let Just (_, implt) = shadowImpl^.S.implOperations^.at name
   Just shadowDyn <- makeShadow bop
   return $ unsafePerformIO $ R.runDynCell shadowImpl implt shadowDyn

-- | Given a BufferedOperation (which for example may have failed its guard)
-- free its arguments back to into the correct state
freeArgs :: BufferedOperation -> GenState ()
freeArgs bop =
      if cat == S.Generator then
         return ()
      else do
         _ <- sequence $ map (freeArg (f cat)) (dugArgs bop)
         return ()
   where cat = bop^.bufOp^.S.opCategory
         f S.Observer = observers
         f S.Mutator = mutators

freeArg :: Lens' GenSt NodeBucket -> D.DUGArg -> GenState ()
freeArg m (S.NonVersion _) = return () -- Nothing to free
freeArg m (S.Version i) = do
   m . persistents %= MSt.delete i
   pers <- use (m . persistents)
   if not (MSt.member i pers) then
      m . infants %= MSt.insert i
   else return ()


-- | Satisfying an NVA is non-trivial, we assume we've already fixed version arguments
-- all that's left is to generate some non-version ones.  However an arbtirary set of non-version arguments
-- may be invalid, so we iterate passing the (unevaluated) args to the shadow to ask if they're valid.
-- TODO: determine when to fail out of iteration
tryFillNVAs :: BufferedOperation -> GenState ()
tryFillNVAs bop = do
      args <- sequence $ map (trySatisfyNVArg) (bop^.bufArgs)
      let bop' = bop{_bufArgs=args}
      result <- runShadow bop'
      case result of
         R.RunSuccess _ -> pushBuffer bop'
         R.RunExcept R.NotImplemented -> pushBuffer bop'
         R.RunTypeMismatch -> error "Shadow type mismatch"
         R.RunExcept R.GuardFailed -> freeArgs bop' >> pushBuffer bop

-- | Satisfy as many of the operation's abstract version arguments as possible
-- then place it back on the buffer.
trySatisfyAndPush :: BufferedOperation -> GenState ()
trySatisfyAndPush bop = do
      args <- sequence $ map (trySatisfyArg k) (bop^.bufArgs)
      pushBuffer $ bop{_bufArgs=args}
   where k = bop^.bufOp^.S.opCategory

-- | Satisfying each version argument involves pulling a node from the correct "bucket" of infant/persistent nodes.
trySatisfyArg :: S.OperationCategory -> BufferedArg -> GenState BufferedArg
trySatisfyArg _          (arg@(Concrete _)) = return arg
trySatisfyArg _          (arg@(Abstract (S.NonVersion _) _)) = return arg
trySatisfyArg S.Mutator  (aty@(Abstract (S.Version ()) p)) = satisfyVersionArg aty p mutators
trySatisfyArg S.Observer (aty@(Abstract (S.Version ()) p)) = satisfyVersionArg aty p observers

-- | Try satisfy a non-version argument
trySatisfyNVArg (arg@(Concrete _)) = return arg
trySatisfyNVArg (arg@(Abstract (S.NonVersion nva) _)) = satisfyNVA nva
trySatisfyNVArg _ = error "trySatisfyNVArg :: passed a non- non-version-arg"

satisfyNVA :: S.NVA () () () -> GenState BufferedArg
satisfyNVA (S.IntArg _) = do
   i <- R.genRandomR (-10, 10)
   return $ Concrete (S.NonVersion (S.IntArg i))
satisfyNVA (S.BoolArg _) = do
   i <- R.genRandomR (True, False)
   return $ Concrete (S.NonVersion (S.BoolArg i))
satisfyNVA (S.VersionParam _) = do  -- Monomorphise to Int
   i <- R.genRandomR (-10, 10)
   return $ Concrete (S.NonVersion (S.VersionParam i))

infixr 6 <||>
m1 <||> m2 = do
   v <- m1
   case v of
      Nothing -> m2
      Just _ -> return v

satisfyVersionArg :: BufferedArg -> PersistenceType -> Lens' GenSt NodeBucket -> GenState BufferedArg
satisfyVersionArg aty Persistent bag = do
   arg <-
      join $ R.genUniform
               [ pullFromInfants bag <||> pullFromPersistents bag
               , pullFromPersistents bag
               ]
   case arg of
      Nothing -> return aty
      Just nodeId -> return $ Concrete (S.Version nodeId)
satisfyVersionArg aty Ephemeral bag = do
   arg <- pullFromInfants bag
   case arg of
      Nothing -> return aty
      Just nodeId -> return $ Concrete (S.Version nodeId)

-- | Pull a nodeId from the infants set
-- if no such node exists, then return Nothing
getBag :: Lens' GenSt NodeBucket -> Lens' NodeBucket (MSt.MSet Int) -> GenState (St.Set Int)
getBag m k = do
   bg <- use (m . k)
   living <- use living
   return $ living `St.intersection` (MSt.toSet bg)

pullFromInfants :: Lens' GenSt NodeBucket -> GenState (Maybe Int)
pullFromInfants m = do
   infs <- getBag m infants
   case St.size infs of
      0 -> return Nothing
      _ -> do
         x <- R.genUniformSet infs
         m . infants %= MSt.delete x
         return $ Just x

-- | Pull a nodeId from the infants set and push it into the persistents set
-- if no such node exists, then return Nothing
pullFromInfantsPersistent :: Lens' GenSt NodeBucket -> GenState (Maybe Int)
pullFromInfantsPersistent m = do
   infs <- getBag m infants
   case St.size infs of
      0 -> return Nothing
      _ -> do
         x <- R.genUniformSet infs
         m . infants %= MSt.delete x
         m . persistents %= MSt.insert x
         return $ Just x


-- | Peek a nodeId from the persistents set
-- if no such node exists, then return Nothing
pullFromPersistents :: Lens' GenSt NodeBucket -> GenState (Maybe Int)
pullFromPersistents m = do
   pers <- getBag m persistents
   case St.size pers of
      0 -> return Nothing
      _ -> do
         v <- R.genUniformSet pers
         return $ Just v
