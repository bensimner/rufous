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
import qualified Test.Rufous.Options as Opt
import qualified Test.Rufous.Profile as P

import Test.Rufous.Internal.Generate.Types
import Test.Rufous.Internal.Generate.Buffer
import qualified Test.Rufous.Internal.Generate.Random as Rnd
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
   o <- Rnd.genAccordingTo (st^.profile^.P.operationWeights) (st ^. sig ^. S.operations)
   args <- sequence $ genAbstractArgs o
   updateDbg inflatedOps (M.insertWith (+) (o^.S.opName) 1)
   bopId <- Rnd.genRandomR (1,100000)
   return $ BufferedOperation bopId o args 100 Nothing args -- TODO: load from options

genAbstractArgs :: S.Operation -> [GenState BufferedArg]
genAbstractArgs o = genAbstractFromArgTy (o^.S.opName) <$> o^.S.opArgTypes

genAbstractFromArgTy :: String -> S.ArgType -> GenState BufferedArg
genAbstractFromArgTy opName ty = do
   prof <- use profile
   pers <- Rnd.genBool ((prof^.P.persistentApplicationWeights) M.! opName)
   return $ Abstract ty (if pers then Persistent else Ephemeral)

tryDeflate :: GenState Bool
tryDeflate = do
   updateDbg deflateSteps (+1)
   bops <- popBufferAll
   bs <- mapM tryDeflateBop bops
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
         -- attach the shadow
         -- we do this so that we can run it once
         -- and re-use the same eval'd cell
         shadow <- makeShadow bop

         evalShadow <- genOpt Opt.genEvalShadow
         if not evalShadow then
            cont shadow
         else
            checkShadow shadow
   where
      checkShadow shadow = do
         let bop' = bop{_bufShadow=shadow}
         result <- runShadow bop'
         case result of
            R.RunSuccess _ -> cont shadow
            R.RunExcept R.NotImplemented -> cont shadow
            R.RunTypeMismatch -> error "Shadow type mismatch"
            R.RunExcept R.GuardFailed -> do
               -- record for each Version that it failed for this arg.
               -- and for versions that reached a threshold, don't try those
               -- Versions again for this operation
               -- debugTrace $ "tryFillNVAs :: guardFailed (" ++ show bop ++ ")"
               let vs = [v | Concrete (S.Version v) _ <- bop^.bufArgs]
               mapM_ (tickFailedGuardCount (bop^.bufOp)) vs
               updateDbg failedGuards (+1)
               failure
            _ -> error "Rufous: internal: commitBop unreachable shadow error state"
      failure = do
         case bop^.life of
            0 -> updateDbg diedOfOldAge (+1)
            i -> pushBuffer bop{_life=i-1, _bufArgs=(bop^.bufAbstractArgs)}
         return False
      cont sh = do
         s <- use sig
         d <- use dug
         let nodeId = D.nextId d
         dug %= D.pushNew (bop^.bufOp) (dugArgs bop) sh
         nodeCounts %= M.insert nodeId 0
         failedApplicationCount %= M.insert nodeId 0
         prof <- use profile
         alive <- Rnd.genBool (1 - prof^.P.mortality)
         if bop^.bufOp^.S.opCategory /= S.Observer && alive then do
            living %= LSt.addNewInfant s nodeId
         else return ()
         commitDargs bop
         updateDbg committedOps (M.insertWith (+) (bop^.bufOp^.S.opName) 1)
         return True

killForOp :: S.Operation -> Int -> GenState ()
killForOp o n = do
   living %= LSt.deleteOp (o^.S.opName) n
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
      mapM_ (commitDarg bop) (bop^.bufArgs)

-- | Given a BufferedArg, commit it
commitDarg :: BufferedOperation -> BufferedArg -> GenState ()
commitDarg _ (Abstract _ _) = error "commitDarg :: unexpected abstract arg"
commitDarg bop (Concrete d pty) =
   case pty of
      Nothing -> return ()
      Just Persistent -> commitPersistent bop d
      Just Ephemeral -> commitEphemeral d

commitPersistent :: BufferedOperation -> D.DUGArg -> GenState ()
commitPersistent bop (S.NonVersion _) = error "commitPersistent :: unexpected Non-Version argument"
commitPersistent bop (S.Version v) = do
   living %= LSt.unuse v

commitEphemeral :: D.DUGArg -> GenState ()
commitEphemeral (S.NonVersion _) = error "commitEphemeral :: unexpected Non-Version argument"
commitEphemeral (S.Version v) = do
   living %= LSt.unuse v

dugArgs :: BufferedOperation -> [D.DUGArg]
dugArgs bop = [a | Concrete a _ <- bop^.bufArgs]

makeShadow :: BufferedOperation -> GenState (Maybe Dynamic)
makeShadow bop = do
   d <- use dug
   s <- use sig
   case s^.S.shadowImpl of
      Just shadow -> return $ Just $ R.makeShadowDynCell s shadow d (bop^.bufOp) (dugArgs bop)
      Nothing     -> return $ Nothing


-- | run the bop's shadow and see what happens
-- unsafe!  do not apply this on buffered operations
-- who have no shadows.
runShadow :: BufferedOperation -> GenState (R.RunResult)
runShadow bop = do
   s <- use sig
   let o = bop^.bufOp
   let name = o^.S.opName
   case bop^.bufShadow of
      Just shadowDyn -> do
         let Just shadowImpl = s^.S.shadowImpl
         let Just (_, implt) = shadowImpl^.S.implOperations^.at name
         return $ unsafePerformIO $ R.runDynCell s o shadowImpl undefined implt shadowDyn Nothing
      Nothing ->
         return $ R.RunSuccess ()

-- | Satisfying an NVA is non-trivial, we assume we've already fixed version arguments
-- all that's left is to generate some non-version ones.  However an arbtirary set of non-version arguments
-- may be invalid, so we iterate passing the (unevaluated) args to the shadow to ask if they're valid.
-- TODO: determine when to fail out of iteration
tryFillNVAs :: BufferedOperation -> GenState ()
tryFillNVAs bop = do
      args <- sequence $ map (trySatisfyNVArg) (bop^.bufArgs)
      let bop' = bop{_bufArgs=args}
      pushBuffer bop'

-- | For a given nodeId increment its failedApplicationCount
-- and if it's reached the threshold, remove it from this operation's
-- living set
tickFailedGuardCount :: S.Operation -> Int -> GenState ()
tickFailedGuardCount op n = do
   failedApplicationCount %= M.update (Just . (+1)) n
   counts <- use failedApplicationCount
   timeout <- genOpt Opt.genFailGuardTimeout
   if counts M.! n > timeout then do
      --debugTrace $ "tickFailedGuardCount [" ++ op^.S.opName ++ "]"
      killForOp op n
   else
      return ()

-- | Satisfy as many of the operation's abstract version arguments as possible
-- then place it back on the buffer.
trySatisfyAndPush :: BufferedOperation -> GenState ()
trySatisfyAndPush bop = do
      args <- mapM (trySatisfyArg bop k) (bop^.bufArgs)
      pushBuffer $ bop{_bufArgs=args}
   where k = bop^.bufOp^.S.opCategory

-- | Satisfying each version argument involves pulling a node from the correct "bucket" of infant/persistent nodes.
trySatisfyArg :: BufferedOperation -> S.OperationCategory -> BufferedArg -> GenState BufferedArg
trySatisfyArg _ _          (arg@(Concrete _ _)) = return arg
trySatisfyArg _ _          (arg@(Abstract (S.NonVersion _) _)) = return arg
trySatisfyArg bop S.Mutator  (aty@(Abstract (S.Version ()) p)) = satisfyVersionArg bop aty p
trySatisfyArg bop S.Observer (aty@(Abstract (S.Version ()) p)) = satisfyVersionArg bop aty p
trySatisfyArg _ S.Generator (Abstract (S.Version _) _) = error "Rufous: internal: trySatisfy version arg to generator: unreachable"

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
   v <- Rnd.genArbitrary tproxy
   return $ wrapNva (S.ArbArg x v Nothing)
satisfyNVA (S.ArbArg _ _ (Just _)) =
   error "Rufous: generation ArbArg got Just for arb type,  expected Nothing."
satisfyNVA (S.VersionParam _) = do  -- Concretize to Int
   i <- Rnd.genRandomR (-10, 10)
   return $ wrapNva (S.VersionParam i)

satisfyVersionArg :: BufferedOperation -> BufferedArg -> PersistenceType -> GenState BufferedArg
satisfyVersionArg bop aty Persistent = do
   arg <- pickFromPersistents bop
   case arg of
      Nothing -> do
         -- debugTrace $ "satisfyVersion Persistent,  noLivingNodes :: " ++ bop^.bufOp^.S.opName
         updateDbg noLivingNodes (+1)
         return aty
      Just nodeId ->
         return $ Concrete (S.Version nodeId) (Just Persistent)
satisfyVersionArg bop aty Ephemeral = do
   arg <- pickFromInfants bop
   case arg of
      Nothing -> do
         -- debugTrace $ "satisfyVersion Ephemeral,  noLivingNodes :: " ++ bop^.bufOp^.S.opName
         updateDbg noLivingNodes (+1)
         return aty
      Just nodeId ->
         return $ Concrete (S.Version nodeId) (Just Ephemeral)

-- | Non-destructively pick an argument from the infants bag
pickFromInfants :: BufferedOperation -> GenState (Maybe Int)
pickFromInfants bop = do
   alive <- use living
   case LSt.imageUnusedInf (bop^.bufOp^.S.opName) alive of
      s | St.size s > 0 -> do
         x <- Rnd.genUniformSet s
         living %= LSt.useInf x
         return $ Just x
      _ ->
         case LSt.imageInf (bop^.bufOp^.S.opName) alive of
            s | St.size s > 0 -> do
               x <- Rnd.genUniformSet s
               living %= LSt.useInf x
               return $ Just x
            _ -> return Nothing

-- | Non-destructively pick an argument from either the persistents or infants bag
pickFromPersistents :: BufferedOperation -> GenState (Maybe Int)
pickFromPersistents bop = do
   alive <- use living
   case LSt.imageUnused (bop^.bufOp^.S.opName) alive of
      s | St.size s > 0 -> do
         x <- Rnd.genUniformSet s
         living %= LSt.usePers x
         return $ Just x
      _ ->
         case LSt.imageUnusedInf (bop^.bufOp^.S.opName) alive of
            s | St.size s > 0 -> do
               x <- Rnd.genUniformSet s
               living %= LSt.useInf x
               return $ Just x
            _ ->
               case LSt.imageInf (bop^.bufOp^.S.opName) alive of
                  s | St.size s > 0 -> do
                     x <- Rnd.genUniformSet s
                     living %= LSt.useInf x
                     return $ Just x
                  _ ->
                     case LSt.image (bop^.bufOp^.S.opName) alive of
                        s | St.size s > 0 -> do
                           x <- Rnd.genUniformSet s
                           living %= LSt.usePers x
                           return $ Just x
                        _ -> return Nothing

