{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.TH (makeADTSignature) where

import Control.Lens
import qualified Data.Map as M

import Data.List (isPrefixOf)
import Data.Dynamic (toDyn)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (showName)

import Test.Rufous.Signature
import Test.Rufous.Extract
import Test.Rufous.Run

import Control.Exception

-- Classifying a list of args into a type is straightforward:

isVersion (Version v) = True
isVersion _ = False

classifyArgs :: [ArgType] -> OperationCategory
classifyArgs args =
   if not . isVersion $ last args then
      Observer
   else
      if any isVersion (init args) then
         Mutator
      else
         Generator

-- makeADTSignature will extract information about a given class
makeADTSignature :: Name -> DecsQ
makeADTSignature name = do
   info <- reify name
   case info of
      ClassI (ClassD ctx name tys funds sigds) insts -> do
         let tyPairs = pairsFromSigds sigds
         let opPairs = pairsToQ tyPairs
         let ops = [| M.fromList $opPairs |]

         -- Collect all visible implementations
         let implBuilders0 = mkImplBuilders tyPairs insts
         let (maybeShadow, implBuilders) = selectBuilders implBuilders0 -- Extract the Shadow
         let impls = buildImpls implBuilders -- build the rest into Q Dec's

         -- Make WrappedADT versions of each of them
         extractorImpls <- mkExtractorImpls name tyPairs implBuilders

         -- Build the Null-implementation
         let (nullImpl, qnullDecl) = mkNullImpl name tyPairs
         nullDecl <- qnullDecl
         let shadow = case maybeShadow of
               Nothing      -> [| Nothing |]
               Just builder -> [| Just $(buildImpl builder) |]

         shadowTy <- case maybeShadow of
               Nothing      -> VarT <$> newName "a"
               Just (ty, _) -> return $ AppT ty (TupleT 0)

         -- Finally build the Test.Rufous.Signature.Signature declaration
         nullExtractDecl <- mkExtractorImpl name tyPairs nullImpl
         let nullExtract = mkImplBuilder tyPairs nullExtractDecl
         let sig = [| Signature $ops $impls $(buildImpl nullImpl) $(buildImpl nullExtract) $shadow  |]
         let specName = mkName $ "_" ++ (nameBase name)
         let specPat = return $ VarP specName
         ds <- [d| $specPat = $sig |]

         -- Return all declarations
         return $ nullDecl : nullExtractDecl : ds ++ extractorImpls
      _ -> fail "makeADTSignature expected class name as argument"

selectBuilders :: [InstanceBuilder] -> (Maybe InstanceBuilder, [InstanceBuilder])
selectBuilders insts = select isShadowImpl insts

select :: (a -> Bool) -> [a] -> (Maybe a, [a])
select f things = go things []
   where
      go [] acc = (Nothing, acc)
      go (x:xs) acc = if f x then (Just x, acc ++ xs) else go xs (acc ++ [x])

type Pair = (String, [ArgType])
type Pairs = [Pair]

pairsFromSigds :: [Dec] -> Pairs
pairsFromSigds [] = []
pairsFromSigds ((SigD name ty) : decls) = pairFromSigd name ty : pairsFromSigds decls

pairFromSigd :: Name -> Type -> (String, [ArgType])
pairFromSigd name ty = (nameBase name, argsFromType ty)

argsFromType :: Type -> [ArgType]
argsFromType ty =
   case ty of
      ForallT _ _ ty -> argsFromType ty  -- unwrap the forall on the class constraint
      AppT (AppT ArrowT lhs) rhs -> argFromType lhs : argsFromType rhs
      x                          -> [argFromType x]

argFromType :: Type -> ArgType
argFromType ty =
   case ty of
      AppT (VarT ctorName) (VarT varName) -> Version ()
      ConT name -> 
         case showName name of
            "GHC.Types.Int" -> NonVersion (IntArg ())
            "GHC.Types.Bool" -> NonVersion (BoolArg ())
            nm -> error $ "argFromType :: Unexpected type constructor " ++ nm
      VarT name -> NonVersion (VersionParam ())
      x -> error $ "argFromType :: Unexpected value " ++ show x

expsToExp :: [Q Exp] -> Q Exp
expsToExp [] = [| [] |]
expsToExp (e:es) = [| ($e) : ($(expsToExp es)) |]

pairsToQ :: Pairs -> Q Exp
pairsToQ ps = expsToExp $ map pairToQ $ ps

pairToQ :: (String, [ArgType]) -> Q Exp
pairToQ (name, args) = do 
   let var = return $ LitE $ StringL name
   let mkArg (Version ()) = [| Version () |]
       mkArg (NonVersion (VersionParam _)) = [| NonVersion (VersionParam ()) |]
       mkArg (NonVersion (IntArg _)) = [| NonVersion (IntArg ()) |]
       mkArg (NonVersion (BoolArg _)) = [| NonVersion (BoolArg ()) |]
   let mkArgs []     = [| [] |] 
       mkArgs (x:xs) = [| $(mkArg x) : $(mkArgs xs) |]
   let (args', classify, retArg') = (mkArgs (init args), classifyArgs args, mkArg (last args))
   let mkClassifier' Mutator = [| Mutator |]
       mkClassifier' Generator = [| Generator |]
       mkClassifier' Observer = [| Observer |]
   [| ($var, Operation $var $retArg' $args' $(mkClassifier' classify)) |]

-- i.e. ("[]", [("snoc", a->t a->t a, t a)])
type InstanceBuilder = (Type, [(String, Type, Type)])

mkNullImpl :: Name -> Pairs -> (InstanceBuilder, Q Dec)
mkNullImpl className pairs = (inst, q)
   where
      inst = (ConT ''Null, mkImplBuilderVars (ConT ''Null) pairs)
      q = do
         ds <- mkNullImplFromPairs pairs
         return $ InstanceD Nothing [] (AppT (ConT className) (ConT ''Null)) ds

mkNullImplFromPairs :: Pairs -> Q [Dec]
mkNullImplFromPairs [] = return $ []
mkNullImplFromPairs (p:ps) = do
   decl <- mkNullImplFromPair p 
   remain <- mkNullImplFromPairs ps
   return $ decl ++ remain

mkNullImplFromPair :: Pair -> Q [Dec]
mkNullImplFromPair (name, args) = do
   let name' = (mkName name)
   let patterns = map (^. _1) $ mkPats (init args)
   if isVersion . last $ args then do
      null' <- [| NullImpl |]
      return $ [FunD name' [Clause patterns (NormalB $ null') []]]
   else do
      impl <- [| throw NotImplemented |]
      return  $ [FunD name' [Clause patterns (NormalB $ impl) []]]

mkPats :: [ArgType] -> [(Pat, Arg Name Name Name Name)]
mkPats ats = go ats 0
   where
      go [] _ = []
      go (arg:args) k = 
         let name' = mkName $ name k 
         in (VarP name', mkArg arg name') : go args (k + 1)
      name k = "x" ++ show k
      mkArg (Version _) name = Version name
      mkArg (NonVersion (VersionParam _)) name = NonVersion (VersionParam name)
      mkArg (NonVersion (IntArg _)) name = NonVersion (IntArg name)
      mkArg (NonVersion (BoolArg _)) name = NonVersion (BoolArg name)

mkExtractorImpls :: Name -> Pairs -> [InstanceBuilder] -> Q [Dec]
mkExtractorImpls className pairs impls = sequence . map (mkExtractorImpl className pairs) $ impls

mkExtractorImpl :: Name -> Pairs -> InstanceBuilder -> Q Dec
mkExtractorImpl className pairs (instTy, _) = q
   where
      q = do
         ds <- mkExtractorImplFromPairs pairs
         return $ InstanceD Nothing [] (AppT (ConT className) (AppT (ConT ''WrappedADT) instTy)) ds

mkExtractorImplFromPairs :: Pairs -> Q [Dec]
mkExtractorImplFromPairs [] = return $ []
mkExtractorImplFromPairs (p:ps) = do
   decl <- mkExtractorImplFromPair p 
   remain <- mkExtractorImplFromPairs ps
   return $ decl ++ remain

mkExtractorImplFromPair :: Pair -> Q [Dec]
mkExtractorImplFromPair (name, args) = do
   let name' = (mkName name)
   let nameLit = return $ LitE $ StringL name
   let patterns = mkPats (init args)
   -- i.e. ``f x y = _log_operation "f" [NonVersion x, Version y] (f (x) (getVersion (y)))``
   let pats = map (^. _1) patterns
   let call = buildCall name' patterns
   let versionExps = patsToVersions patterns
   let versionsExp = expsToExp versionExps
   if not . isVersion . last $ args then do
      impl' <- [| _log_observer $nameLit $versionsExp $call |] 
      return $ [FunD name' [Clause pats (NormalB $ impl') []]]
   else do
      impl' <- [| _log_operation $nameLit $versionsExp $call |] 
      return $ [FunD name' [Clause pats (NormalB $ impl') []]]

buildCall :: Name -> [(Pat, Arg Name Name Name Name)] -> Q Exp
buildCall n xs = go xs [| $(return $ VarE n) |]
   where
      go [] f = f
      go ((_, arg) : cs) f = go cs [| $f $(mkArg arg) |]
      mkArg (Version v) = [| getVersion $(return $ VarE $ v) |]
      mkArg (NonVersion (VersionParam v)) = [| $(return $ VarE $ v) |]
      mkArg (NonVersion (IntArg v)) = [| $(return $ VarE $ v) |]
      mkArg (NonVersion (BoolArg v)) = [| $(return $ VarE $ v) |]
      mkArg (NonVersion v) = error $ "buildCall :: got unexpected mkArg arg"


patsToVersions :: [(Pat, Arg Name Name Name Name)] -> [Q Exp]
patsToVersions xs = do
   (VarP n, a) <- xs
   let ne = return $ VarE n
   case a of
      Version n    -> return [| Version $ne |]
      NonVersion (VersionParam n) -> return [| NonVersion (VersionParam $ne) |]
      NonVersion (IntArg n) -> return [| NonVersion (IntArg $ne) |]
      NonVersion (BoolArg n) -> return [| NonVersion (BoolArg $ne) |]

isShadowImpl :: InstanceBuilder -> Bool
isShadowImpl (ty, _) = 
   case ty of
      ConT name -> "Shadow" `isPrefixOf` nameBase name
      x         -> False

mkImplBuilders :: Pairs -> [InstanceDec] -> [InstanceBuilder]
mkImplBuilders pairs [] = []
mkImplBuilders pairs (inst:insts) = mkImplBuilder pairs inst : mkImplBuilders pairs insts

mkImplBuilder :: Pairs -> InstanceDec -> InstanceBuilder
mkImplBuilder pairs (InstanceD Nothing _ (AppT _ ty) _) =
   (ty, mkImplBuilderVars ty pairs)

mkImplBuilderVars :: Type -> Pairs -> [(String, Type, Type)]
mkImplBuilderVars ty [] = [] 
mkImplBuilderVars ty (p:ps) = mkImplBuilderVar ty p : mkImplBuilderVars ty ps

mkImplBuilderVar :: Type -> Pair -> (String, Type, Type)
mkImplBuilderVar ty (name, args) = (name, argTysToType ty args, aTypeToType ty finalArg)
   where
      finalArg = last args

buildImpls :: [InstanceBuilder] -> Q Exp
buildImpls [] = [| [] |]
buildImpls (impl:impls) = [| $(buildImpl impl) : $(buildImpls impls) |]

buildImpl :: InstanceBuilder -> Q Exp
buildImpl (conTy, pairs) = [| Implementation $(ctorNameStr) (M.fromList $(buildImplPairs pairs)) |]
   where ctorNameStr = return $ LitE $ StringL (userfriendlyTypeString conTy)

buildImplPairs :: [(String, Type, Type)] -> Q Exp
buildImplPairs [] = [| [] |]
buildImplPairs (pair:pairs) = [| $(buildImplPair pair) : $(buildImplPairs pairs) |]

buildImplPair :: (String, Type, Type) -> Q Exp
buildImplPair (name, ty, retTy) = [| ($nameStr, ($var, $rt)) |]
   where
      nameStr = return $ LitE $ StringL name
      var = return $ AppE (VarE 'toDyn) (SigE (VarE (mkName name)) ty)
      rt = return $ AppE (ConE 'ImplType) (SigE (VarE $ mkName "undefined") retTy)

-- Convert Version/NonVersion arguments to proper (concrete) Type expressions
-- todo: This needs to be more principled, this translation turns everything not-concrete into `Int'
-- which isn't quite right ...
argTysToType :: Type -> [ArgType] -> Type
argTysToType ty [retTy]   = aTypeToType ty retTy
argTysToType ty (aty:tys) = AppT (AppT ArrowT (aTypeToType ty aty)) (argTysToType ty tys)

aTypeToType :: Type -> ArgType -> Type
aTypeToType ty (Version ()) = AppT ty (ConT (mkName "Int"))
aTypeToType ty (NonVersion x) = ConT (mkName "Int")

userfriendlyTypeString :: Type -> String
userfriendlyTypeString (ConT name) = showName name
userfriendlyTypeString t = show t
