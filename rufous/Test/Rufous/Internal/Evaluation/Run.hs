{-# LANGUAGE BangPatterns #-}
module Test.Rufous.Internal.Evaluation.Run where

import Control.Lens

import Control.Exception
import Data.Dynamic

import Data.Time.Clock

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromJust)

import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Internal.DUG.HsPrinter as DP

import Test.Rufous.Internal.Evaluation.Types
import qualified Test.Rufous.Internal.DUG.Spanning as MST

-- | Run a DUG on some null implementation and a list of implementations
-- and return the new annotated DUG with the timing info
run :: S.Signature -> D.DUG -> S.Implementation -> [S.Implementation] -> IO Result
run s d nullImpl impls = do
   nullT <- runOn s d nullImpl
   implTs <- mapM (runOn s d) impls
   let tinfo = TInfo nullT (M.fromList (zip impls implTs))
   let extractedProfile = D.extractProfile s d
   let opCounts = M.empty
   return $ Result d extractedProfile opCounts tinfo

-- | check that the extracted shadows for non-observer nodes matches those of observer nodes
checkDugShadows :: S.Signature -> D.DUG -> S.Implementation -> IO RunResult
checkDugShadows s dug impl = collect <$> (sequence $ check <$> versions)
   where
      ignore !x = return ()
      versions = filter (not . isObserver) $ M.elems $ dug^.D.operations
      isObserver n = n^.D.operation^.S.opCategory == S.Observer
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
         runDynCell s o impl n ity valueDyn (Just (shadowDyn, shadowIty))

      collect :: [RunResult] -> RunResult
      collect [] = error "Rufous: internal: cannot check DUG shadows, did not generate any versions to check!"
      collect [r] = r
      collect (RunSuccess _ : rs) = collect rs
      collect (RunExcept NotImplemented : rs) = collect rs
      collect (x : _) = x

runOn :: S.Signature -> D.DUG -> S.Implementation -> IO NominalDiffTime
runOn s d impl = do
      t0 <- getCurrentTime
      _ <- observed
      t1 <- getCurrentTime

      r <- checkDugShadows s runDUG impl
      case r of
         RunSuccess _ -> return (diffUTCTime t1 t0)
         RunShadowFailure i n _ _ -> error $
            "Rufous: shadow and extracted shadow did not match for node " ++ DP.defn n
            ++ ",  on implementation " ++ show i ++ "."
         -- These should never occur if using the TH splices
         RunShadowTypeMismatch -> error "Rufous: internal: shadow type mismatch."
         RunTypeMismatch -> error "Rufous: internal: type mismatch."
         RunExcept e -> error $  "Rufous: internal: " ++ show e

   where observers :: [D.Node]
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
   makeDynCellGen s impl d o args (\d -> fromJust (d^.D.dyn))

makeShadowDynCell :: S.Signature -> S.Implementation -> D.DUG -> S.Operation -> [D.DUGArg] -> Dynamic
makeShadowDynCell s impl d o args =
   makeDynCellGen s impl d o args (\d -> d^.D.shadow)

makeDynCellGen :: S.Signature -> S.Implementation -> D.DUG -> S.Operation -> [D.DUGArg] -> (D.Node -> Dynamic) -> Dynamic
makeDynCellGen s impl d o args fn = dynResult f dynArgs
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
runDynCell s o i n (S.ImplType retT) d maybeShadow = do
      res <- runDyn retT fromDynamic d
      case res of
         RunSuccess r ->
            case forcer s o of
               Nothing -> tryCheckShadow r
               Just (dynforcer, S.ImplType t') -> do
                  r' <- runDyn t' fromDynamic (dynforcer `dynApp` d)
                  case r' of
                     RunSuccess r'' -> tryCheckShadow r''
                     x -> return x
         x -> return x
   where
      runDyn :: Typeable a => a -> (Dynamic -> Maybe a) -> Dynamic -> IO RunResult
      runDyn _ f d' = do
         case f d' of
            Nothing -> return $ RunTypeMismatch
            Just r  -> catch (wrap r) handleE
      handleE :: RufousException -> IO RunResult
      handleE = return . RunExcept
      wrap !x = return $ RunSuccess x
      shadowImplementation = fromJust (s^.S.shadowImpl)
      tryCheckShadow r' =
         case (o^.S.opCategory, maybeShadow, i^.S.shadowExtractor) of
            -- Only run the shadow extraction on generators/mutators
            (S.Observer, _, _) -> return $ RunSuccess r'
            -- and only if there's a shadow type
            (_, Nothing, _) -> return $ RunSuccess r'
            -- and only if there's a shadow extraction function to go with it ...
            (_, Just _, Nothing) -> return $ RunSuccess r'
            -- then apply the shadow extraction function to the concrete value
            (_, Just (sh, S.ImplType shT), Just shExtr) -> do
               -- and compare it to the Node's own calculated Shadow during generation
               checkShadow (shExtr `dynApp` d) sh r'
            _ -> fail "unreachable"
      checkShadow :: Typeable b => Dynamic -> Dynamic -> b -> IO RunResult
      checkShadow s extractedS ret =
         case shadowImplementation^.S.implEq of
            Just eqDyn ->
               case fromDynamic (eqDyn `dynApp` s `dynApp` extractedS) of
                  Nothing    -> return $ RunShadowTypeMismatch
                  Just v     -> catch (return $ if v then RunSuccess ret else RunShadowFailure i n "" "")  handleE
            Nothing    -> return $ RunSuccess ret