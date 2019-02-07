module Test.Rufous.Internal.Evaluation.Run where

import Control.Lens

import Control.Exception
import Data.Dynamic

import Data.Time.Clock
import Data.Maybe

import qualified Data.Map as M

import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.DUG as D

import Test.Rufous.Internal.Evaluation.Types

run :: D.DUG -> S.Implementation -> IO NominalDiffTime
run d impl = do
      obs <- observed
      return $ sum [t | (_, t) <- obs]
   where observers :: [D.Node]
         observers = filter isObserver $ runDUG ^. D.operations ^.. traverse
         runDUG = buildImplDUG impl d
         isObserver :: D.Node -> Bool
         isObserver n = n^.D.operation^.S.opCategory == S.Observer
         observed = sequence $ map observe observers
         observe :: D.Node -> IO (RunResult, NominalDiffTime)
         observe n = do
            t0 <- getCurrentTime
            let Just (_, ity) = impl ^. S.implOperations . at (n^.D.operation^.S.opName)
            res <- runDynCell impl ity (n^.D.shadow)
            t1 <- getCurrentTime
            let d = diffUTCTime t1 t0
            case res of
               RunSuccess _ -> return (res, d)
               RunTypeMismatch -> error "Type mismatch!"
               RunExcept e -> error (show e) -- TODO: rufous exceptions?

buildImplDUG :: S.Implementation -> D.DUG -> D.DUG
buildImplDUG impl d = newdug
   where newdug = d & D.operations %~ M.map (updateNode impl newdug)

updateNode :: S.Implementation -> D.DUG -> D.Node -> D.Node
updateNode impl d n = n{D._shadow=dyn}
   where dyn = makeDynCell impl d (n^.D.operation) (n^.D.args)

makeDynCell :: S.Implementation -> D.DUG -> S.Operation -> [D.DUGArg] -> Dynamic
makeDynCell impl d op args = dynResult f as
   where f :: Dynamic
         Just (f, retTy) = impl ^. S.implOperations ^. at (op^.S.opName)
         dynResult f [] = f
         dynResult f (a:as) = dynResult (f `dynApp` a) as
         as :: [Dynamic]
         as = do
            arg <- args
            case arg of
               S.Version i -> do 
                  let Just n = d^.D.operations . at i
                  return $ n ^. D.shadow
               S.NonVersion (S.IntArg i) -> return $ toDyn i
               S.NonVersion (S.BoolArg b) -> return $ toDyn b
               S.NonVersion (S.VersionParam k) -> return $ toDyn (k :: Int)


runDynCell :: S.Implementation -> S.ImplType -> Dynamic -> IO RunResult
runDynCell impl (S.ImplType t) d = run t fromDynamic
   where run :: Typeable a => a -> (Dynamic -> Maybe a) -> IO RunResult
         run _ f = do
            case f d of
               Nothing -> return $ RunTypeMismatch
               Just r  -> catch' $ r `seq` RunSuccess r
         handleE :: RufousException -> IO RunResult
         handleE = return . RunExcept
         catch' x = catch (return x) handleE
