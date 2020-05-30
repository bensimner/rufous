{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.TH (makeADTSignature, makeExtractors) where

import System.IO.Unsafe

import Control.Lens
import qualified Data.Map as M

import Data.List (isPrefixOf, intercalate)
import Data.Dynamic (toDyn)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (showName)

import Test.Rufous.Signature
import Test.Rufous.Extract
import Test.Rufous.Run

import Control.Exception

{-
Template Haskell is used to auto-generate the `Signature` for a given ADT,
and, if wished, the DUG-extracting implementations

> class ListADT t where
>    ...
>
> makeADTSignature ''ListADT
> makeExtractors ''ListADT

It has 4 stages:
   1. Collection of the ADT (class) itself
      a) collect all type family definitions used
      b) collect all operations
      c) for each operation, generate the Simple grammar that corresponds to it
   2. Collection of implementations
      for each known instance of the class:
         a) extract the name
         b) wrap each operation into a dynamic cell
   3. Generate Null implementation from the class
      a) for each Generator and Mutator generate an operation that returns a Null
      b) for each Observer generate an operation that raises NotImplemented
   4. Generate extraction implementations:
      for each instance of the class except the generated Null instance:
         for each method of the class:
            a) wrap the method of the instance with a side-effectful logger that
               for generators/mutators returns a WrappedADT by wrapping the result of applying the instance method
               for observers returns the underlying value by applying the instance method
            b) construct a new (WrappedADT OtherInstance) instance

`makeADTSignature` then puts the results of all of these together into one `Signature` object.
-}


-- | information about a given ADT as a Typeclass
-- we attach the class Name
-- but we also attach the type variable name for the container
-- e.g. `class Queue q` => ADTDef ''Queue ''q _
data ADTDef = ADTClass
   { adtClsName :: Name
   , adtBoundVar :: Name  -- TODO: Maybe Name ?

   -- | to hnadle higher-kinded types like Map k v we have an interface class Mappable v with type family Key k
   -- so we store here that Key is one of the defined type family ctors,  which should later be interpreted as a NV_c(Int)
   , adtTyFamilyCtors :: [Name]
   }
   deriving (Show)


-- | An implementation definition information
-- an instance defines an implementation, and there are broadly two types kinds:
-- so-called 'first-order' ones: e.g. Queue, Set, List etc
-- and higher-order ones whose constructors take extra parameters,  tuples, maps etc
-- in theory a (Map k) and a Queue aren't much different,  but the TH macro has to generate different code
-- so we distinguish them here ...
data IMPLDef =
   -- e.g. Instance Queue [] ...
     FirstOrder Cxt Type --  FirstOrder ''Queue

   -- e.g. for instance Mappable (Map k) ... with type Key (Map k) = k
   | SecondOrder Cxt [Name] Type  -- SecondOrder [Key] (D.M.Map k)
   deriving (Show)

parseName :: Name -> Q (TyVarBndr, [Dec], [InstanceDec])
parseName name = do
   info <- reify name
   case info of
      ClassI (ClassD [] _ [v] _ sigds) insts ->
         return (v, sigds, insts)
      ClassI (ClassD [] _ (_:_:_) _ _) _ -> fail $
         "Rufous: multiple bound type variables in class declaration not supported."
      ClassI (ClassD (_:_) _ _ _ _) _ -> fail $
         "Rufous: constraints in class declaration not supported."
      i -> fail $
         "Rufous: makeADTSignaure expected type class name as argument, not a "
         ++ friendlyInfoName i ++ " name"

-- | `makeADTSignature` is a Template Haskell macro that automatically produces the Signature
-- for a given ADT defined as a typeclass.
makeADTSignature :: Name -> DecsQ
makeADTSignature name = do
   (v, sigds, insts) <- parseName name
   boundTyVarName <- tyVarName v
   let tyfamilies = [n | (OpenTypeFamilyD (TypeFamilyHead n _ _ _)) <- sigds]
   let adt = ADTClass name boundTyVarName tyfamilies

   opInfoPairs <- opPairsFromMethSigs adt name sigds  -- extract method information
   let ops = [| M.fromList $(pairsToOpExprs opInfoPairs) |]

   -- Collect all visible implementations
   implBuilders0 <- mkImplBuilders adt opInfoPairs insts
   let (maybeShadow, implBuilders) = select isShadowImpl implBuilders0
   let impls = buildImpls implBuilders

   -- Build the Null-implementation
   let (nullInstanceBuilder, qnullDecl) = mkNullImpl adt opInfoPairs
   nullDecl <- qnullDecl

   let shadow = case maybeShadow of
         Nothing      -> [| Nothing |]
         Just builder -> [| Just $(buildImpl builder) |]

   -- Finally build the Signature declaration
   nullImplBuilder <- mkImplBuilder adt opInfoPairs nullDecl
   let n = return $ LitE $ StringL $ nameBase name
   let sig = [| Signature $n $ops $impls $(buildImpl nullInstanceBuilder) $(buildImpl nullImplBuilder) $shadow  |]
   let specName = mkName $ "_" ++ (nameBase name)
   let specPat = return (VarP specName)
   ds <- [d| $specPat = $sig |]
   sigTy <- [t| Signature |]
   let ts = [SigD specName sigTy]

   -- Return all declarations
   return $ nullDecl : ts ++ ds


-- | constructs the extractor implementations for each known implementation
-- of the given typeclass, automatically
makeExtractors :: Name -> DecsQ
makeExtractors name = do
   (v, sigds, insts) <- parseName name
   boundTyVarName <- tyVarName v
   let tyfamilies = [n | (OpenTypeFamilyD (TypeFamilyHead n _ _ _)) <- sigds]
   let adt = ADTClass name boundTyVarName tyfamilies

   opInfoPairs <- opPairsFromMethSigs adt name sigds  -- extract method information

   -- Collect all visible implementations
   implBuilders0 <- mkImplBuilders adt opInfoPairs insts
   let (_, implBuilders) = select isShadowImpl implBuilders0

   -- Make WrappedADT versions of each of them
   extractorImpls <- mkExtractorImpls adt opInfoPairs implBuilders

   -- Return all declarations
   return extractorImpls

tyVarName :: TyVarBndr -> Q Name
tyVarName (PlainTV v) = return v
tyVarName (KindedTV v ((AppT (AppT ArrowT StarT) StarT))) = return v
tyVarName (KindedTV v kind) = fail $
      "Rufous: in binding for type variable " ++ (nameBase v) ++ " in class declaration: "
      ++ (nameBase v) ++ " has unsupported kind " ++ (pprint kind) ++ ".  Only kind * -> * is supported."

select :: (a -> Bool) -> [a] -> (Maybe a, [a])
select f things = go things []
   where
      go [] acc = (Nothing, acc)
      go (x:xs) acc = if f x then (Just x, acc ++ xs) else go xs (acc ++ [x])

type Pair = (String, [ArgType])
type Pairs = [Pair]

opPairsFromMethSigs :: ADTDef -> Name -> [Dec] -> Q Pairs
opPairsFromMethSigs _ _ [] = return []
-- can skip open type families that were collected earlier ...
opPairsFromMethSigs cls methName ((OpenTypeFamilyD _) : decls) =
   opPairsFromMethSigs cls methName decls
opPairsFromMethSigs cls _ ((SigD name ty) : decls) = do
   x <- opPairsFromMethSig cls name ty
   xs <- opPairsFromMethSigs cls name decls
   return (x:xs)
opPairsFromMethSigs _ name (d:_) = fail $
   "Rufous: Declaration in "
   ++ (show name)
   ++ " expected method signature not "
   ++ (friendlyDecName d)
   ++ " in"
   ++ prettyShowDec d

opPairsFromMethSig :: ADTDef -> Name -> Type -> Q (String, [ArgType])
opPairsFromMethSig cls methName ty = do
   args <- argsFromType cls methName ty
   return (nameBase methName, args)

argsFromType :: ADTDef -> Name -> Type -> Q [ArgType]
argsFromType adt methName ty =
   case ty of
      ForallT _ _ ty' -> argsFromType adt methName ty'  -- unwrap the forall on the class constraint
      AppT (AppT ArrowT lhs) rhs -> do
         lhsArg <- argFromType adt methName lhs
         rhsArg <- argsFromType adt methName rhs
         return (lhsArg:rhsArg)
      x  -> sequence [argFromType adt methName x]

argFromType :: ADTDef -> Name -> Type -> Q ArgType
argFromType (ADTClass _ tvar tyfamilies) methName ty =
   case ty of
      -- TODO: check that this is AppT (VarT clsVar)
      AppT (VarT v) (VarT _) | v == tvar -> return $ Version ()
      AppT (ConT v) _ | v `elem` tyfamilies -> return $ NonVersion  (IntArg ())
      ConT name ->
         case showName name of
            "GHC.Types.Int" -> return $ NonVersion (IntArg ())
            "GHC.Types.Bool" -> return $ NonVersion (BoolArg ())
            _ -> fail $
               "Rufous: error in signature for method "
               ++ nameBase methName
               ++ ". "
               ++ "Only concrete types Int and Bool in class method signatures are supported, not "
               ++ (nameBase name)
      VarT _ -> return $ NonVersion (VersionParam ())
      x -> fail $
         "Rufous: non-simple type in signature for " ++ (nameBase methName)
         ++ ": " ++ (pprint x) ++ " is not simple."

listExpr :: [Q Exp] -> Q Exp
listExpr [] = [| [] |]
listExpr (e:es) = [| ($e) : ($(listExpr es)) |]

pairsToOpExprs :: Pairs -> Q Exp
pairsToOpExprs ps = listExpr $ pairToOpExpr <$> ps

pairToOpExpr :: (String, [ArgType]) -> Q Exp
pairToOpExpr (name, args) = do
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


{- |
 An `InstanceBuilder` is all the information required to create a new instance

 e.g. given class ADT t where f :: t a -> a
      then instance ADT Null  where f n = undefined

So nullImplBuilder = (FirstOrder ''ADT ''Null, [("f", [Version ()], NonVersion VA)]

for a more complex example:
   class M m k where f :: k -> v -> m k v -> m k v
   instance Typeable k => M Null k where f m k v = Null

Then mImplBuilder = (IMPLInst [Typeable k] [("f", ...)])
-}
type InstanceBuilder = (IMPLDef, [(String, Type, Type)])

mkNullImpl :: ADTDef -> Pairs -> (InstanceBuilder, Q Dec)
mkNullImpl (ADTClass className _ familyCtors) pairs = (builder, q)
   where
      vars = mkImplBuilderVars (ConT ''Null) pairs
      inst = if null familyCtors then FirstOrder [] (ConT ''Null) else SecondOrder [] familyCtors (ConT ''Null)
      builder = (inst, vars)
      q = do
         ds <- mkNullImplFromPairs pairs
         let tyfamilyDecs = [TySynInstD $ TySynEqn Nothing (AppT (ConT n) (ConT ''Null)) (ConT ''Int) | n <- familyCtors]
         return $ InstanceD Nothing [] (AppT (ConT className) (ConT ''Null)) (tyfamilyDecs ++ ds)

mkNullImplFromPairs :: Pairs -> Q [Dec]
mkNullImplFromPairs [] = return $ []
mkNullImplFromPairs (p:ps) = do
   decl <- mkNullImplFromPair p
   remain <- mkNullImplFromPairs ps
   return $ decl ++ remain

mkNullImplFromPair :: Pair -> Q [Dec]
mkNullImplFromPair (name, args) = do
   let name' = (mkName name)
   let patterns = map (^. _1) $ mkPats False (init args)
   if isVersion . last $ args then do
      null' <- [| NullImpl |]
      return $ [FunD name' [Clause patterns (NormalB $ null') []]]
   else do
      impl <- [| throw NotImplemented |]
      return  $ [FunD name' [Clause patterns (NormalB $ impl) []]]

mkPats :: Bool -> [ArgType] -> [(Pat, Arg Name Name Name Name)]
mkPats readvars ats = go ats (0 :: Int)
   where
      go [] _ = []
      go (arg:args) k =
         let name' = mkName $ name k
         in (if readvars then VarP name' else WildP, mkArg arg name') : go args (k + 1)
      name k = "x" ++ show k
      mkArg (Version _) ident = Version ident
      mkArg (NonVersion (VersionParam _)) ident = NonVersion (VersionParam ident)
      mkArg (NonVersion (IntArg _)) ident = NonVersion (IntArg ident)
      mkArg (NonVersion (BoolArg _)) ident = NonVersion (BoolArg ident)

mkExtractorImpls :: ADTDef -> Pairs -> [InstanceBuilder] -> Q [Dec]
mkExtractorImpls cls pairs impls = sequence . map (mkExtractorImpl cls pairs) $ impls

extractorTy :: ADTDef -> Type -> Q Type
extractorTy (ADTClass nm _ _) ty = return $ AppT (ConT nm) (AppT (ConT ''WrappedADT) (concretize ty))

mkExtractorImpl :: ADTDef -> Pairs -> InstanceBuilder -> Q Dec
mkExtractorImpl cls pairs (FirstOrder _ ty, _) = do
   ds <- mkExtractorImplFromPairs pairs
   ety <- extractorTy cls ty
   return $ InstanceD Nothing [] ety ds
mkExtractorImpl cls pairs (SecondOrder _ families ty, _) = do
   ds <- mkExtractorImplFromPairs pairs
   ety@(AppT _ instTy) <- extractorTy cls ty
   let tyfamilyDecs = [TySynInstD $ TySynEqn Nothing (AppT (ConT n) instTy) (ConT ''Int) | n <- families]
   return $ InstanceD Nothing [] ety (tyfamilyDecs ++ ds)

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
   let patterns = mkPats True (init args)
   let pats = map (^. _1) patterns
   let call = buildCall name' (zip [0..] patterns)
   if not . isVersion . last $ args then do
      impl' <- [| unsafePerformIO $ _get_id >>= (\curId -> return (_log_observer curId $nameLit $(call [| curId |] ) )) |]
      return $ [FunD name' [Clause pats (NormalB $ impl') []]
               , PragmaD (InlineP name' NoInline FunLike AllPhases)]
   else do
      impl' <- [| unsafePerformIO $ _get_id >>= (\curId -> return (_log_operation curId $nameLit $(call [| curId |] ) )) |]
      return $ [FunD name' [Clause pats (NormalB $ impl') []]
               , PragmaD (InlineP name' NoInline FunLike AllPhases)]

buildCall :: Name -> [(Integer, (Pat, Arg Name Name Name Name))] -> Q Exp -> Q Exp
buildCall n xs curId = go xs [| $(return $ VarE n) |]
   where
      go [] f = f
      go ((i, (_, arg)) : cs) f = go cs [| $f $(mkArg i arg) |]
      mkArg i (Version v) =
            let ivar = return $ LitE $ IntegerL i in
            [| unwrap $curId $ivar $(return $ VarE $ v) |]
      mkArg i (NonVersion (VersionParam v)) =
            let var = return $ VarE $ v in
            let ivar = return $ LitE $ IntegerL i in
            [| nonversion $curId $ivar (NonVersion (VersionParam $var)) $var  |]
      mkArg i (NonVersion (IntArg v)) =
            -- TODO: is this right?  It was 'fail'd out
            let var = return $ VarE $ v in
            let ivar = return $ LitE $ IntegerL i in
            [| nonversion $curId $ivar (NonVersion (IntArg $var)) $var  |]
      mkArg _ (NonVersion (BoolArg _)) = fail "Fatal error generating extracted ADT: Boolean Args not implemented"

isShadowImpl :: InstanceBuilder -> Bool
isShadowImpl (t, _) =
   case t of
      FirstOrder _ ty -> go ty
      SecondOrder _ _ ty -> go ty
   where
      go ty =
         case ty of
            ConT name -> "Shadow" `isPrefixOf` nameBase name
            AppT a _ -> go a
            _ -> False


mkImplBuilders :: ADTDef -> Pairs -> [InstanceDec] -> Q [InstanceBuilder]
mkImplBuilders _ _ [] = return []
mkImplBuilders cls pairs insts = sequence $ map (mkImplBuilder cls pairs) insts

mkImplBuilder :: ADTDef -> Pairs -> InstanceDec -> Q InstanceBuilder
mkImplBuilder (ADTClass _ _ []) pairs (InstanceD Nothing cxts (AppT (ConT _) c) _) =
   return (FirstOrder cxts c, mkImplBuilderVars c pairs)
-- TODO: More specific errors ...
mkImplBuilder (ADTClass _ _ tyvars) pairs (InstanceD Nothing cxts (AppT (ConT _) c) _) =
   return (SecondOrder cxts tyvars c, mkImplBuilderVars c pairs)
mkImplBuilder _ _ i@(InstanceD _ _ _ _) = fail $
   "Rufous: unsupported instance declaration at " ++ prettyShowDec i
mkImplBuilder _ _ d = fail $
   "Rufous: expected instance declaration, not " ++ prettyShowDec d

mkImplBuilderVars :: Type -> Pairs -> [(String, Type, Type)]
mkImplBuilderVars _ [] = []
mkImplBuilderVars ty (p:ps) = mkImplBuilderVar ty p : mkImplBuilderVars ty ps

mkImplBuilderVar :: Type -> Pair -> (String, Type, Type)
mkImplBuilderVar ty (name, args) = (name, argTysToType ty args, aTypeToType ty finalArg)
   where
      finalArg = last args

buildImpls :: [InstanceBuilder] -> Q Exp
buildImpls [] = [| [] |]
buildImpls (impl:impls) = [| $(buildImpl impl) : $(buildImpls impls) |]

buildImpl :: InstanceBuilder -> Q Exp
-- TODO: Cxt ?  e.g. what to do about `instance Ord k => A (Foo k)` ?
buildImpl (FirstOrder _ ty, pairs) = [| Implementation $(ctorNameStr) (M.fromList $(buildImplPairs pairs)) |]
   where ctorNameStr = return $ LitE $ StringL (userfriendlyTypeString ty)
buildImpl (SecondOrder _ _ ty, pairs) = [| Implementation $(ctorNameStr) (M.fromList $(buildImplPairs pairs)) |]
   where ctorNameStr = return $ LitE $ StringL (userfriendlyTypeString ty)

buildImplPairs :: [(String, Type, Type)] -> Q Exp
buildImplPairs [] = [| [] |]
buildImplPairs (pair:pairs) = [| $(buildImplPair pair) : $(buildImplPairs pairs) |]

buildImplPair :: (String, Type, Type) -> Q Exp
buildImplPair (name, ty, retTy) = [| ($nameStr, ($var, $rt)) |]
   where
      nameStr = return $ LitE $ StringL name
      var = return $ AppE (VarE 'toDyn) (SigE (VarE (mkName name)) (concretize ty))
      rt = return $ AppE (ConE 'ImplType) (SigE (VarE $ mkName "undefined") (concretize retTy))

-- Convert Version/NonVersion arguments to proper (concrete) Type expressions
-- todo: This needs to be more principled, this translation turns everything not-concrete into `Int'
-- which isn't quite right ...
argTysToType :: Type -> [ArgType] -> Type
argTysToType ty [retTy]   = aTypeToType ty retTy
argTysToType ty (aty:tys) = AppT (AppT ArrowT (aTypeToType ty aty)) (argTysToType ty tys)
argTysToType _ [] = error "argTysToType :: empty type signature"  -- this should not actually be reachable ?

aTypeToType :: Type -> ArgType -> Type
aTypeToType ty (Version ()) = AppT (concretize ty) (ConT (mkName "Int"))
aTypeToType _ (NonVersion (VersionParam _)) = ConT (mkName "Int")
aTypeToType _ (NonVersion (BoolArg _)) = ConT (mkName "Bool")
aTypeToType _ (NonVersion _) = ConT (mkName "Int")

-- concretize C a -> C Int
concretize :: Type -> Type
concretize (VarT _) = ConT (mkName "Int")
concretize (AppT c1 c2) = AppT (concretize c1) (concretize c2)
concretize x = x

userfriendlyTypeString :: Type -> String
-- expand lhs ...
userfriendlyTypeString (AppT t1 _) = userfriendlyTypeString t1
-- ... until we reach concrete type
userfriendlyTypeString (ConT name) = showName name
userfriendlyTypeString t = show t


-- * for pretty error printing
-- this stuff is pretty important

friendlyInfoName :: Info -> String
friendlyInfoName (ClassI _ _) = "class"
friendlyInfoName (ClassOpI _ _ _) = "class method"
friendlyInfoName (TyConI _) = "type constructor"
friendlyInfoName (FamilyI _ _) = "type/data family"
friendlyInfoName (PrimTyConI _ _ _) = "type constructor"
friendlyInfoName (DataConI _ _ _) = "data constructor"
friendlyInfoName (PatSynI _ _) = "pattern synonym"
friendlyInfoName (VarI _ _ _) = "value variable"
friendlyInfoName (TyVarI _ _) = "type variable"

friendlyDecName :: Dec -> String
friendlyDecName (FunD _ _) = "function declaration"
friendlyDecName (ValD _ _ _) = "value declaration"
friendlyDecName (DataD _ _ _ _ _ _) = "datatype declaration"
friendlyDecName (NewtypeD _ _ _ _ _ _) = "newtype declaration"
friendlyDecName (TySynD _ _ _) = "type synonym declaration"
friendlyDecName (ClassD _ _ _ _ _) = "typeclass declaration"
friendlyDecName (InstanceD _ _ _ _) = "instance declaration"
friendlyDecName (SigD _ _) = "function signature"
friendlyDecName (InfixD _ _) = "infix declaration"
friendlyDecName (TySynInstD _) = "type synonym instance declaration"
friendlyDecName (DefaultSigD _ _) = "default function signature"
friendlyDecName (OpenTypeFamilyD _) = "open type family declaration"
friendlyDecName x = "Unknown? (" ++ show x ++ ")"

prettyShowDec :: Dec -> String
prettyShowDec d = sep ++ intercalate sep (pad (lines (pprint d))) ++ "\n     "
   where pad xs = "" : (xs ++ [""])
         sep = "\n   | "