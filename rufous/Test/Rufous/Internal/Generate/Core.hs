{-# LANGUAGE RankNTypes #-}
module Test.Rufous.Internal.Generate.Core where

import Control.Lens

import Data.Dynamic
import System.IO.Unsafe

import Control.Monad.State (get)

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
import qualified Test.Rufous.Internal.Generate.LivingSet as LSt

-- | Build will run the inflate/deflate cycle calling inflate 'size' times.
build :: Int -> GenState D.DUG
build initSize = go initSize
   where
      go 0 = do
         verboseProgress (initSize `mod` 100)
         use dug

      go size = do
         inflate
         b <- tryDeflate
         debugIf b flatDeflateSteps (+1)
         d <- use dug
         buf <- use buffer
         alive <- use living
         debug <- use dbg
         if size `mod` 100 == 0 then do
            verboseProgress 100
            debugTrace $ "Step "
               ++ "("
               ++ "remaining=" ++ show size
               ++ ", size=" ++ show (D.size d)
               ++ ", #buf=" ++ show (length buf)
               ++ ", #alive=" ++ show (LSt.size alive)
               ++ ", dbg=" ++ prettyDebugInfo debug
               ++ ")"
         else return ()
         go (size - 1)

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
   o <- R.genAccordingTo (st^.profile^.P.operationWeights) (st ^. sig ^. S.operations)
   args <- sequence $ genAbstractArgs o
   updateDbg inflatedOps (M.insertWith (+) (o^.S.opName) 1)
   return $ BufferedOperation o args 100 -- TODO: load from options

genAbstractArgs :: S.Operation -> [GenState BufferedArg]
genAbstractArgs o = genAbstractFromArgTy (o^.S.opName) <$> o^.S.opArgTypes

genAbstractFromArgTy :: String -> S.ArgType -> GenState BufferedArg
genAbstractFromArgTy opName ty = do
   prof <- use profile
   pers <- R.genBool ((prof^.P.persistentApplicationWeights) M.! opName)
   return $ Abstract ty (if pers then Persistent else Ephemeral)

tryDeflate :: GenState Bool
tryDeflate = do
   updateDbg deflateSteps (+1)
   bops <- popBufferAll
   bs <- sequence $ map tryDeflateBop bops
   if or bs then do -- if we successfully created any new versions
      _ <- tryDeflate    -- then loop to see if we can satisfy any more
      return True
   else
      return False

-- | Try to "deflate"  (i.e. fill and/or commit) a buffered operation.
-- if success then add new node directly to DUG, and return True
-- otherwise it places it back on the buffer and returns False.
tryDeflateBop :: BufferedOperation -> GenState Bool
tryDeflateBop bop | satisifed bop          = commitBop bop
tryDeflateBop bop | partiallySatisifed bop = tryFillNVAs bop       >> return False
tryDeflateBop bop                          = trySatisfyAndPush bop >> return False


-- | "commit" a buffered operation; adding it to the partially-built DUG.
commitBop :: BufferedOperation -> GenState Bool
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
            R.RunExcept R.GuardFailed -> do
               _ <- sequence $ do
                  Concrete (S.Version v) _ <- bop^.bufArgs
                  return $ do
                     nc <- use nodeCounts
                     let Just k = M.lookup v nc
                     if k > 200 then --  TODO: move to options/settings
                        kill v
                     else
                        nodeCounts %= M.update (Just . (+1)) v
               updateDbg failedGuards (+1)
               failure
            _ -> error "Rufous: internal: commitBop unreachable shadow error state"
   where
      failure = do
         case bop^.life of
            0 -> updateDbg diedOfOldAge (+1)
            i -> abstractify bop{_life=i-1} >>= pushBuffer -- maybe just drop?
         return False
      cont = do
         s <- use sig
         d <- use dug
         let nodeId = D.nextId d
         shadow <- makeShadow bop
         dug %= D.pushNew (bop^.bufOp) (dugArgs bop) shadow
         nodeCounts %= M.insert nodeId 0
         prof <- use profile
         alive <- R.genBool (1 - prof^.P.mortality)
         if bop^.bufOp^.S.opCategory /= S.Observer && alive then do
            living %= LSt.insertAll s nodeId
         else return ()
         commitDargs bop
         mutators  . infants %= MSt.insert nodeId
         observers . infants %= MSt.insert nodeId
         updateDbg committedOps (M.insertWith (+) (bop^.bufOp^.S.opName) 1)
         return True

kill :: Int -> GenState ()
kill n = do
   updateDbg deadNodes (+1)
   s <- use sig
   living %= LSt.deleteAll s n

killForOp :: S.Operation -> Int -> GenState ()
killForOp o n = do
   living %= LSt.delete (o^.S.opName) n
   alive <- use living

   if not (n `LSt.memberAll` alive) then
      updateDbg deadNodes (+1)
   else
      return ()

-- | Check all BufferedArg's point to a living node
checkDargs :: BufferedOperation -> GenState Bool
checkDargs bop =  do
   alive <- use living
   let checks = map (checkDarg alive) (bop^.bufArgs)
   return $ and checks

checkDarg :: LSt.LivingSet -> BufferedArg -> Bool
checkDarg alive  (Concrete (S.Version v) _) = v `LSt.memberAll` alive
checkDarg _      (Concrete (S.NonVersion _) _) = True
checkDarg _      (Abstract _ _) = error "checkDarg :: unexpected abstract arg"

-- | Given a (committed) BufferedOperation, commit the arguments
commitDargs :: BufferedOperation -> GenState ()
commitDargs bop = do
      mapM_ (commitDarg bop m) (bop^.bufArgs)
   where m :: Lens' GenSt NodeBucket
         m = case bop^.bufOp^.S.opCategory of
             S.Mutator -> mutators
             S.Observer -> observers
             S.Generator -> error "commitDargs :: Generator has unexpected version arg"

-- | Given a BufferedArg, commit it
commitDarg :: BufferedOperation -> Lens' GenSt NodeBucket -> BufferedArg -> GenState ()
commitDarg _ _ (Abstract _ _) = error "commitDarg :: unexpected abstract arg"
commitDarg bop m (Concrete d pty) =
   case pty of
      Nothing -> return ()
      Just Persistent -> commitPersistent bop m d
      Just Ephemeral -> commitEphemeral m d

commitPersistent :: BufferedOperation -> Lens' GenSt NodeBucket -> D.DUGArg -> GenState ()
commitPersistent bop _ (S.NonVersion _) = error "commitPersistent :: unexpected Non-Version argument"
commitPersistent bop m (S.Version v) = do
   infs <- getBag bop m infants
--   pers <- getBag bop m persistents
   if v `St.member` infs then do
      m . infants %= MSt.delete v
      m . persistents %= MSt.insert v
   else return ()

commitEphemeral :: Lens' GenSt NodeBucket -> D.DUGArg -> GenState ()
commitEphemeral _ (S.NonVersion _) = error "commitEphemeral :: unexpected Non-Version argument"
commitEphemeral _ (S.Version v) = return ()

abstractify :: BufferedOperation -> GenState BufferedOperation
abstractify bop = do
   args <- sequence $ genAbstractArgs (bop^.bufOp)
   return bop{_bufArgs=args}

dugArgs :: BufferedOperation -> [D.DUGArg]
dugArgs bop = [a | Concrete a _ <- bop^.bufArgs]

makeShadow :: BufferedOperation -> GenState Dynamic
makeShadow bop = do
   d <- use dug
   s <- use sig
   let Just shadow = s^.S.shadowImpl
   return $ R.makeShadowDynCell s shadow d (bop^.bufOp) (dugArgs bop)

runShadow :: BufferedOperation -> GenState (R.RunResult)
runShadow bop = do
   s <- use sig
   let o = bop^.bufOp
   let name = o^.S.opName
   let Just shadowImpl = s^.S.shadowImpl
   let Just (_, implt) = shadowImpl^.S.implOperations^.at name
   shadowDyn <- makeShadow bop
   return $ unsafePerformIO $ R.runDynCell s o shadowImpl undefined implt shadowDyn Nothing


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
         R.RunExcept R.GuardFailed -> do
            -- record for each Version that it failed for this arg.
            -- and for versions that reached a threshold, don't try those
            -- Versions again for this operation
            let vs = [v | Concrete (S.Version v) _ <- bop^.bufArgs]
            mapM_ (tickFailedGuardCount (bop^.bufOp)) vs
            pushBuffer bop
         R.RunTypeMismatch -> error "Rufous: internal: shadow type mismatch"
         _ -> error "Rufous: internal: tryFillNVAs unreachable shadow error state"

-- | For a given nodeId increment its failedApplicationCount
-- and if it's reached the treshhold, remove it from this operation's
-- living set
tickFailedGuardCount :: S.Operation -> Int -> GenState ()
tickFailedGuardCount op n = do
   failedApplicationCount %= M.update (Just . (+1)) n
   counts <- use failedApplicationCount
   if counts M.! n > 100 then do
      debugTrace $ "tickFailedGuardCount [" ++ op^.S.opName ++ "]"
      killForOp op n
   else
      return ()

-- | Satisfy as many of the operation's abstract version arguments as possible
-- then place it back on the buffer.
trySatisfyAndPush :: BufferedOperation -> GenState ()
trySatisfyAndPush bop = do
      args <- sequence $ map (trySatisfyArg bop k) (bop^.bufArgs)
      pushBuffer $ bop{_bufArgs=args}
   where k = bop^.bufOp^.S.opCategory

-- | Satisfying each version argument involves pulling a node from the correct "bucket" of infant/persistent nodes.
trySatisfyArg :: BufferedOperation -> S.OperationCategory -> BufferedArg -> GenState BufferedArg
trySatisfyArg _ _          (arg@(Concrete _ _)) = return arg
trySatisfyArg _ _          (arg@(Abstract (S.NonVersion _) _)) = return arg
trySatisfyArg bop S.Mutator  (aty@(Abstract (S.Version ()) p)) = satisfyVersionArg bop aty p mutators
trySatisfyArg bop S.Observer (aty@(Abstract (S.Version ()) p)) = satisfyVersionArg bop aty p observers
trySatisfyArg _ S.Generator (Abstract (S.Version _) _) = error "trySatisfy version arg to generator :: impossible by defn"

-- | Try satisfy a non-version argument
trySatisfyNVArg :: BufferedArg -> GenState BufferedArg
trySatisfyNVArg (arg@(Concrete _ _)) = return arg
trySatisfyNVArg (Abstract (S.NonVersion nva) _) = satisfyNVA nva
trySatisfyNVArg _ = error "trySatisfyNVArg :: passed a non- non-version-arg"

-- TODO: This. Is. Terrible.
wrapNva :: S.NVA Int () -> BufferedArg
wrapNva n = Concrete (S.NonVersion n) Nothing

satisfyNVA :: S.NVA () () -> GenState BufferedArg
satisfyNVA (S.ArbArg x tproxy Nothing) = do
   v <- R.genArbitrary tproxy
   return $ wrapNva (S.ArbArg x v Nothing)
satisfyNVA (S.ArbArg _ _ (Just _)) =
   error "Rufous: generation ArbArg got Just for arb type,  expected Nothing."
satisfyNVA (S.VersionParam _) = do  -- Concretize to Int
   i <- R.genRandomR (-10, 10)
   return $ wrapNva (S.VersionParam i)

satisfyVersionArg :: BufferedOperation -> BufferedArg -> PersistenceType -> Lens' GenSt NodeBucket -> GenState BufferedArg
satisfyVersionArg bop aty Persistent bag = do
   arg <- pickFromPersistents bop bag
   case arg of
      Nothing -> do
         -- debugTrace $ "satisfyVersion Persistent,  noLivingNodes :: " ++ bop^.bufOp^.S.opName
         updateDbg noLivingNodes (+1)
         return aty
      Just nodeId -> return $ Concrete (S.Version nodeId) (Just Persistent)
satisfyVersionArg bop aty Ephemeral bag = do
   arg <- pickFromInfants bop bag
   case arg of
      Nothing -> do
         -- debugTrace $ "satisfyVersion Ephemeral,  noLivingNodes :: " ++ bop^.bufOp^.S.opName
         updateDbg noLivingNodes (+1)
         return aty
      Just nodeId -> return $ Concrete (S.Version nodeId) (Just Ephemeral)

-- | Pull a nodeId from the infants set
-- if no such node exists, then return Nothing
getBag :: BufferedOperation -> Lens' GenSt NodeBucket -> Lens' NodeBucket (MSt.MSet Int) -> GenState (St.Set Int)
getBag bop m k = do
   bg <- use (m . k)
   alive <- use living
   let aliveForBop = LSt.image (bop^.bufOp^.S.opName) alive
   return $ aliveForBop `St.intersection` (MSt.toSet bg)

-- | Non-destructively pick an argument from the infants bag
pickFromInfants :: BufferedOperation -> Lens' GenSt NodeBucket -> GenState (Maybe Int)
pickFromInfants bop m = do
   infs <- getBag bop m infants
   case St.size infs of
      0 -> return Nothing
      _ -> do
         x <- R.genUniformSet infs
         return $ Just x

-- | Non-destructively pick an argument from either the persistents or infants bag
pickFromPersistents :: BufferedOperation -> Lens' GenSt NodeBucket -> GenState (Maybe Int)
pickFromPersistents bop m = do
   pers <- getBag bop m persistents
   infs <- getBag bop m infants
   let merged = pers `St.union` infs
   case St.size merged of
      0 -> return Nothing
      _ -> do
         x <- R.genUniformSet merged
         return $ Just x

