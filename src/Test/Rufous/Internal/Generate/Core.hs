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
tryDeflateBop bop | satisifed bop          = commitBop bop         >> return True
tryDeflateBop bop | partiallySatisifed bop = tryFillNVAs bop       >> return False
tryDeflateBop bop                          = trySatisfyAndPush bop >> return False


-- | "commit" a buffered operation; adding it to the partially-built DUG.
commitBop :: BufferedOperation -> GenState ()
commitBop bop = do
      validArgs <- checkDargs bop
      if not validArgs then 
         failure
      else do
         result <- runShadow bop
         case result of
            R.RunSuccess _ -> cont
            R.RunExcept R.NotImplemented -> cont
            R.RunTypeMismatch -> error "Shadow type mismatch"
            R.RunExcept R.GuardFailed -> failure
   where
      failure =
         case bop^.life of
            0 -> return ()
            i -> abstractify bop{_life=i-1} >>= pushBuffer
      cont = do
         d <- use dug
         let nodeId = D.nextId d
         Just shadow <- makeShadow bop
         dug %= D.pushNew (bop^.bufOp) (dugArgs bop) shadow
         prof <- use profile
         alive <- R.genBool (1 - prof^.P.mortality)
         if bop^.bufOp^.S.opCategory /= S.Observer && alive then
            living %= St.insert nodeId
         else return ()
         commitDargs bop
         mutators  . infants %= MSt.insert nodeId
         observers . infants %= MSt.insert nodeId

-- | Check all BufferedArg's point to a living node
checkDargs :: BufferedOperation -> GenState Bool
checkDargs bop =  do
   living <- use living
   let checks = map (checkDarg living) (bop^.bufArgs)
   return $ and checks

checkDarg :: St.Set Int -> BufferedArg -> Bool
checkDarg living (Concrete (S.Version v) _) = v `St.member` living
checkDarg _      (Concrete (S.NonVersion _) _) = True
checkDarg _      (Abstract _ _) = error "checkDarg :: unexpected abstract arg"

-- | Given a (committed) BufferedOperation, commit the arguments
commitDargs :: BufferedOperation -> GenState ()
commitDargs bop = do
      mapM_ (commitDarg m) (bop^.bufArgs)
   where m :: Lens' GenSt NodeBucket
         m = case bop^.bufOp^.S.opCategory of
             S.Mutator -> mutators
             S.Observer -> observers
             S.Generator -> error "commitDargs :: Generator has unexpected version arg"

-- | Given a BufferedArg, commit it
commitDarg :: Lens' GenSt NodeBucket -> BufferedArg -> GenState ()
commitDarg m (Abstract _ _) = error "commitDarg :: unexpected abstract arg"
commitDarg m (Concrete d pty) =
   case pty of
      Nothing -> return ()
      Just Persistent -> commitPersistent m d
      Just Ephemeral -> commitEphemeral m d

commitPersistent :: Lens' GenSt NodeBucket -> D.DUGArg -> GenState ()
commitPersistent m (S.NonVersion _) = error "commitPersistent :: unexpected Non-Version argument"
commitPersistent m (S.Version v) = do
   infs <- getBag m infants
   pers <- getBag m persistents
   if v `St.member` infs then do
      m . infants %= MSt.delete v
      m . persistents %= MSt.insert v
   else return ()

commitEphemeral :: Lens' GenSt NodeBucket -> D.DUGArg -> GenState ()
commitEphemeral m (S.NonVersion _) = error "commitEphemeral :: unexpected Non-Version argument"
commitEphemeral m (S.Version v) = do
   living %= St.delete v

abstractify :: BufferedOperation -> GenState BufferedOperation
abstractify bop = do
   args <- sequence $ genAbstractArgs (bop^.bufOp)
   return bop{_bufArgs=args}

dugArgs :: BufferedOperation -> [D.DUGArg]
dugArgs bop = [a | Concrete a _ <- bop^.bufArgs]

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
         R.RunExcept R.GuardFailed -> pushBuffer bop

-- | Satisfy as many of the operation's abstract version arguments as possible
-- then place it back on the buffer.
trySatisfyAndPush :: BufferedOperation -> GenState ()
trySatisfyAndPush bop = do
      args <- sequence $ map (trySatisfyArg k) (bop^.bufArgs)
      pushBuffer $ bop{_bufArgs=args}
   where k = bop^.bufOp^.S.opCategory

-- | Satisfying each version argument involves pulling a node from the correct "bucket" of infant/persistent nodes.
trySatisfyArg :: S.OperationCategory -> BufferedArg -> GenState BufferedArg
trySatisfyArg _          (arg@(Concrete _ _)) = return arg
trySatisfyArg _          (arg@(Abstract (S.NonVersion _) _)) = return arg
trySatisfyArg S.Mutator  (aty@(Abstract (S.Version ()) p)) = satisfyVersionArg aty p mutators
trySatisfyArg S.Observer (aty@(Abstract (S.Version ()) p)) = satisfyVersionArg aty p observers

-- | Try satisfy a non-version argument
trySatisfyNVArg (arg@(Concrete _ _)) = return arg
trySatisfyNVArg (arg@(Abstract (S.NonVersion nva) _)) = satisfyNVA nva
trySatisfyNVArg _ = error "trySatisfyNVArg :: passed a non- non-version-arg"

-- TODO: This. Is. Terrible.
nva n = Concrete (S.NonVersion n) Nothing
satisfyNVA :: S.NVA () () () -> GenState BufferedArg
satisfyNVA (S.IntArg _) = do
   i <- R.genRandomR (-10, 10)
   return $ nva (S.IntArg i)
satisfyNVA (S.BoolArg _) = do
   i <- R.genRandomR (True, False)
   return $ nva (S.BoolArg i)
satisfyNVA (S.VersionParam _) = do  -- Monomorphise to Int
   i <- R.genRandomR (-10, 10)
   return $ nva (S.VersionParam i)

infixr 6 <||>
m1 <||> m2 = do
   v <- m1
   case v of
      Nothing -> m2
      Just _ -> return v

satisfyVersionArg :: BufferedArg -> PersistenceType -> Lens' GenSt NodeBucket -> GenState BufferedArg
satisfyVersionArg aty Persistent bag = do
   arg <- pickFromPersistents bag
   case arg of
      Nothing -> return aty
      Just nodeId -> return $ Concrete (S.Version nodeId) (Just Persistent)
satisfyVersionArg aty Ephemeral bag = do
   arg <- pickFromInfants bag
   case arg of
      Nothing -> return aty
      Just nodeId -> return $ Concrete (S.Version nodeId) (Just Ephemeral)

-- | Pull a nodeId from the infants set
-- if no such node exists, then return Nothing
getBag :: Lens' GenSt NodeBucket -> Lens' NodeBucket (MSt.MSet Int) -> GenState (St.Set Int)
getBag m k = do
   bg <- use (m . k)
   living <- use living
   return $ living `St.intersection` (MSt.toSet bg)

-- | Non-destructively pick an argument from the infants bag
pickFromInfants :: Lens' GenSt NodeBucket -> GenState (Maybe Int)
pickFromInfants m = do
   infs <- getBag m infants
   case St.size infs of
      0 -> return Nothing
      _ -> do
         x <- R.genUniformSet infs
         return $ Just x

-- | Non-destructively pick an argument from either the persistents or infants bag
pickFromPersistents :: Lens' GenSt NodeBucket -> GenState (Maybe Int)
pickFromPersistents m = do
   pers <- getBag m persistents
   infs <- getBag m infants
   let merged = pers `St.union` infs
   case St.size merged of
      0 -> return Nothing
      _ -> do
         x <- R.genUniformSet merged
         return $ Just x

