{-# LANGUAGE BangPatterns #-}
module Test.Rufous.Internal.Evaluation.Run where

import Control.Lens

import Unsafe.Coerce
import Control.Exception
import Data.Dynamic

import Data.Time.Clock

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromJust)

import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Options as Opt
import qualified Test.Rufous.Internal.DUG.HsPrinter as DP

import Test.Rufous.Internal.Evaluation.Types
import Test.Rufous.Internal.Evaluation.Results as Rs
import qualified Test.Rufous.Internal.DUG.Spanning as MST
import qualified Test.Rufous.Internal.Logger as Log

-- | Run a DUG on some null implementation and a list of implementations
-- and return the new annotated DUG with the timing info
run :: S.Signature -> D.DUG -> S.Implementation -> [S.Implementation] -> Int -> IO Result
run s d nullImpl impls i = do
   !results <- mapM (\j -> Log.doIfIO Opt.verbose (Log.updateProgress j i) >> runOnDUG s d nullImpl impls) [1..i]
   Log.doIfIO Opt.verbose Log.endProgress
   let extractedProfile = D.extractProfile s d
   let opCounts = M.empty
   return $ Result d extractedProfile opCounts (foldl1 mergeDUGTimeInfos results) results

-- | Because we might want to time the same DUG multiple times over
-- we have a function that given a (blank) DUG and an implementation
-- it runs it and returns a new result.
runOnDUG :: S.Signature -> D.DUG -> S.Implementation -> [S.Implementation] -> IO DUGTimeInfo
runOnDUG s d nullImpl impls = do
   Log.debug $ "running " ++ d^.D.name
   nullTRun <- runOn s d nullImpl
   case nullTRun of
      -- if failed to evaluate the Null implementation for whatever reason.
      Left f -> failOut f
      Right nullT -> do
         Log.debug $ "running " ++ d^.D.name ++ ", Null time =" ++ show nullT
         implTRuns <- mapM (runOn s d) impls
         case Rs.splitResultFailures implTRuns of
            Left f -> failOut f
            Right implTs -> do
               let tinfo = TInfo nullT (M.fromList (zip impls implTs))
               Log.debug $ "t " ++ d^.D.name ++ ", impl times =" ++ show (zip impls implTs)
               return $ DUGEvalTimes tinfo
   where
      failOut f = return $ DUGEvalFail f

-- | check that the extracted shadows for non-observer nodes matches those of observer nodes
checkDugShadows :: S.Signature -> D.DUG -> S.Implementation -> IO RunResult
checkDugShadows s dug impl = collect <$> checks
   where
      allNodes = M.elems $ dug^.D.operations
      checks = sequence $ (check <$> allNodes)

      -- for each version V with Shadow S_v check if
      --  S_v == extract V
      check n = do
         let Just (_, ity) = impl ^. S.implOperations . at (n^.D.operation^.S.opName)
         let o = n^.D.operation

         -- Get the actual value out of the Node
         let Just valueDyn = n^.D.dyn

         -- extract the Shadow value
         let shadowDyn = n^.D.shadow
         let Just shImpl = s^.S.shadowImpl
         let Just (_, shadowIty) = shImpl ^. S.implOperations . at (n^.D.operation^.S.opName)

         -- run the dynamic cell and check it against the Shadow, timing it
         r <- runDynCell s o impl n ity valueDyn (Just (shadowDyn, shadowIty))
         case r of
            RunSuccess _ -> return r
            RunExcept NotImplemented -> return $ RunSuccess ()
            e -> return e

      collect :: [RunResult] -> RunResult
      collect [] = RunSuccess ()
      collect [r] = r
      collect (RunSuccess _ : rs) = collect rs
      collect (RunExcept NotImplemented : rs) = collect rs
      collect (x : _) = x

runOn :: S.Signature -> D.DUG -> S.Implementation -> IO (Either ResultFailure NominalDiffTime)
runOn s d impl = do
      t0 <- getCurrentTime
      _ <- observed
      t1 <- getCurrentTime

      r <- checkDugShadows s runDUG impl
      case r of
         RunSuccess _ -> return $ Right (diffUTCTime t1 t0)
         RunShadowFailure i n extrShow -> do
            let versions = S.Version (n^.D.nodeId) : [S.Version v | S.Version v <- n^.D.args]
            showVersions <- sequence $ map implNodeShow [v | S.Version v <- versions]
            showShadows <- sequence $ map implNodeShowShadow [v | S.Version v <- versions]
            --showExtractedShadows <- sequence $ map implNodeShowShadow [v | S.Version v <- versions]
            return $ Left $ ResultFail $
               unlines $
                     [
                     "shadow and extracted shadow did not match for node `" ++ DP.showNode n ++ "`"
                     , "for implementation " ++ show i
                     , "where versions: " ]
                  ++ [" " ++ DP.showArg v ++ " = `" ++ ns ++ "`" | (v, ns) <- zip versions showVersions]
                  ++ ["and shadows: "]
                  ++ [" " ++ DP.showArg v ++ " = `" ++ ns ++ "`" | (v, ns) <- zip versions showShadows]
                  ++ ["and extracted shadow: "]
                  ++ [" " ++ DP.showVarName n ++ " = `" ++ extrShow ++ "`"]

         -- These should never occur if using the TH splices
         RunShadowTypeMismatch -> error "Rufous: internal: shadow type mismatch."
         RunTypeMismatch -> error "Rufous: internal: type mismatch during shadow check."
         RunExcept e -> error $  "Rufous: internal: " ++ show e

   where
      observers :: [D.Node]
      observers = filter isObserver $ runDUG ^. D.operations ^.. traverse
      runDUG = buildImplDUG s impl d
      isObserver n = n^.D.operation^.S.opCategory == S.Observer
      observed = sequence $ map observe observers
      observe :: D.Node -> IO (Maybe RunResult)
      observe n = do
         let Just (_, ity) = impl ^. S.implOperations . at (n^.D.operation^.S.opName)
         let o = n^.D.operation
         -- Get the actual value out of the Node
         let Just valueDyn = n^.D.dyn
         -- run the dynamic cell and time it
         res <- runDynCell s o impl n ity valueDyn Nothing
         case res of
            RunSuccess _ -> return (Just res)
            RunExcept NotImplemented -> return Nothing
            e -> error $ "Rufous: internal: " ++ show e
      -- below are for error handling
      implNodeShow :: Int -> IO String
      implNodeShow i =
         case impl^.S.implShow of
            Nothing -> return "<no Show instance>"
            Just m -> do
               let n = (runDUG ^. D.operations) M.! i
               -- we already evaluated the DUG so it better have Dynamic's for all of them ...
               let Just dyn = n ^. D.dyn
               let o = n ^. D.operation ^. S.opName
               let showDyn = m M.! o
               r <- runDyn "" fromDynamic (showDyn `dynApp` dyn)
               case r of
                  RunSuccess str -> return (unsafeCoerce str)
                  x -> error $ "Rufous: internal: cannot show node: " ++ show x
      implNodeShowShadow :: Int -> IO String
      implNodeShowShadow i =
         case s^.S.shadowImpl of
            Nothing -> error "Rufous: internal: cannot show shadow for undefined shadow implementation"
            Just shImpl ->
               case shImpl^.S.implShow of
                  Nothing -> return "<no Show instance>"
                  Just m -> do
                     let n = (runDUG ^. D.operations) M.! i
                     -- we already evaluated the DUG so it better have Dynamic's for all of them ...
                     let dyn = n ^. D.shadow
                     let o = n ^. D.operation ^. S.opName
                     let showDyn = m M.! o
                     r <- runDyn "" fromDynamic (showDyn `dynApp` dyn)
                     case r of
                        RunSuccess str -> return (unsafeCoerce str)
                        x -> error $ "Rufous: internal: cannot show node: " ++ show x

buildImplDUG :: S.Signature -> S.Implementation -> D.DUG -> D.DUG
buildImplDUG s impl d = newdug
   where nodes = MST.orderedMSTFlatten d
         updateDUG oldd n = oldd & D.operations %~ M.insert (n^.D.nodeId) (updateNode s impl oldd n)
         newdug = L.foldl' updateDUG d nodes

updateNode :: S.Signature -> S.Implementation -> D.DUG -> D.Node -> D.Node
updateNode s impl d n = n{D._dyn=Just dyn}
   where dyn = makeDynCell s impl d (n^.D.operation) (n^.D.args)

voidImpl :: S.ImplType
voidImpl = S.ImplType (undefined :: ())

forcer :: S.Signature -> S.Operation -> Maybe (Dynamic, S.ImplType)
forcer s o =
   case M.lookup (o^.S.opName) (s^.S.opObsForcers) of
      Nothing -> Nothing
      Just d -> Just (d, voidImpl)

makeDynCell :: S.Signature -> S.Implementation -> D.DUG -> S.Operation -> [D.DUGArg] -> Dynamic
makeDynCell s impl d o args =
   makeDynCellGen s impl d o args (\n -> fromJust (n^.D.dyn))

makeShadowDynCell :: S.Signature -> S.Implementation -> D.DUG -> S.Operation -> [D.DUGArg] -> Dynamic
makeShadowDynCell s impl d o args =
   makeDynCellGen s impl d o args (\n -> n^.D.shadow)

makeDynCellGen :: S.Signature -> S.Implementation -> D.DUG -> S.Operation -> [D.DUGArg] -> (D.Node -> Dynamic) -> Dynamic
makeDynCellGen _ impl d o args fn = dynResult f dynArgs
   where f :: Dynamic
         Just (f, _) = impl ^. S.implOperations ^. at (o^.S.opName)
         dynResult r [] = r
         dynResult r (a:as) = dynResult (r `dynApp` a) as
         dynArgs :: [Dynamic]
         dynArgs = do
            arg <- args
            case arg of
               S.Version i -> do
                  let Just n = d^.D.operations . at i
                  return $ fn n
               S.NonVersion (S.ArbArg _ v _) ->
                  return $ toDyn v
               S.NonVersion (S.VersionParam k) -> return $ toDyn (k :: Int)

runDynCell :: S.Signature -> S.Operation -> S.Implementation -> D.Node -> S.ImplType -> Dynamic -> Maybe (Dynamic, S.ImplType) -> IO RunResult
runDynCell s o i n (S.ImplType retT) d maybeShadow = result
   where
      result = do
         res <- runDyn retT fromDynamic d
         case res of
            RunSuccess r ->
               case forcer s o of
                  Nothing -> tryCheck r
                  Just (dynforcer, S.ImplType t') -> do
                     r' <- runDyn t' fromDynamic (dynforcer `dynApp` d)
                     case r' of
                        RunSuccess r'' -> tryCheck r''
                        -- NotImplemented == extractorUndefined and so do not try check it
                        RunExcept NotImplemented -> return r'
                        x -> return x
            x -> return x

      shadowImplementation = fromJust (s^.S.shadowImpl)

      -- below are just functions for checking the Shadow
      -- for observers we can simply try check the result directly
      -- for generators/mutators we have to extract the shadow from the result
      -- to compare it.

      tryCheck r' =
         case o^.S.opCategory of
            S.Observer -> tryCheckShadowObs r'
            _ -> tryCheckShadow r'

      tryCheckShadow r' =
         case (maybeShadow, i^.S.shadowExtractor) of
            -- only check the shadow if there's a shadow type defined ...
            (Nothing, _) -> return $ RunSuccess r'
            -- and only if there's a shadow extraction function to go with it ...
            (Just _, Nothing) -> return $ RunSuccess r'
            -- then apply the shadow extraction function to the concrete value
            (Just (sh, _), Just shExtr) -> do
               -- and compare it to the Node's own calculated Shadow during generation
               checkShadow (shExtr `dynApp` d) sh r'

      tryCheckShadowObs r' =
         case maybeShadow of
            Nothing -> return $ RunSuccess r'
            Just (sh, _) -> checkShadowObs sh d r'

      checkShadowObs :: Typeable b => Dynamic -> Dynamic -> b -> IO RunResult
      checkShadowObs shObservation observation ret =
         case i^.S.implEq of
            Just m -> do
               let eqDyn = m M.! (o^.S.opName)
               r <- runDyn False fromDynamic (eqDyn `dynApp` shObservation `dynApp` observation)
               case r of
                  RunSuccess b ->
                     if unsafeCoerce b
                        then return $ RunSuccess ret
                        else shadowFailure shObservation ret
                  RunExcept NotImplemented ->
                     return $ RunSuccess ret
                  x -> return x
            Nothing -> return $ RunSuccess ret

      checkShadow :: Typeable b => Dynamic -> Dynamic -> b -> IO RunResult
      checkShadow shVersion extractedShVersion ret =
         case shadowImplementation^.S.implEq of
            Just m -> do
               let eqDyn = m M.! (o^.S.opName)
               r <- runDyn False fromDynamic (eqDyn `dynApp` shVersion `dynApp` extractedShVersion)
               case r of
                  RunSuccess b ->
                     if unsafeCoerce b
                        then return $ RunSuccess ret
                        else shadowFailure extractedShVersion ret
                  -- forcing s or extractedS could also raise extractorUndefined
                  -- and so that counts as a valid shadow.
                  RunExcept NotImplemented -> return $ RunSuccess ret
                  x -> return $ x
            Nothing -> return $ RunSuccess ret

      shadowFailure extractedShVersion ret =
         case shadowImplementation^.S.implShow of
            Nothing -> return $ RunSuccess ret
            Just m -> do
               let showDyn = m M.! (o^.S.opName)
               showShExtractedV <- runDyn "" fromDynamic (showDyn `dynApp` extractedShVersion)
               case showShExtractedV of
                  RunSuccess s1 ->
                     return $ RunShadowFailure i n (unsafeCoerce s1)
                  _ -> return $ RunShadowFailure i n "N/A"


-- | Run a Dynamic, try force it to WHNF, then return a RunResult
-- which is a:
--  RunTypeMismatch if the dynamic was badly typed
--  RunExcept if forcing the dynamic raises a RufousException
--  RunSuccess v if the force was successful where v is the value it returned
runDyn :: Typeable a => a -> (Dynamic -> Maybe a) -> Dynamic -> IO RunResult
runDyn _ f d' = do
      case f d' of
         Nothing -> return $ RunTypeMismatch
         Just r  -> catch (wrap r) handleE
   where
      handleE :: RufousException -> IO RunResult
      handleE = return . RunExcept
      wrap x = RunSuccess <$> (return $! x)