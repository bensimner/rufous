{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.TH (makeADTSignature, makeExtractors) where

import System.IO.Unsafe

import Control.Lens
import qualified Data.Map as M

import Data.Char (toUpper)
import Data.List (isPrefixOf, intercalate, nub)
import Data.Dynamic (toDyn)
import Data.Typeable (Typeable)

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
   , adtBoundVar :: String

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

implTyCtor :: IMPLDef -> Type
implTyCtor (FirstOrder _ t) = t
implTyCtor (SecondOrder _ _ t) = t

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
   boundTyVarName <- nameBase <$> tyVarName v
   let tyfamilies = [n | (OpenTypeFamilyD (TypeFamilyHead n _ _ _)) <- sigds]
   let adt = ADTClass name boundTyVarName tyfamilies

   -- Collect the ADT methods
   (opInfoPairs, maybeShadowExtractPair) <- opPairsFromMethSigs adt name sigds

   -- collect all the instances of the class
   -- and extract out the implementations from the Shadow (if it exists)
   allImpls <- sequence $ mkImpl adt <$> insts
   let (maybeShadowImpl, implInsts) = select isShadowInst allImpls

   let implBuilders = attachMethods adt opInfoPairs <$> implInsts
   let maybeShadowBuilder = attachMethods adt opInfoPairs <$> maybeShadowImpl

   -- Build the Null-implementation
   let (nullInstanceBuilder, qnullDecl) = mkNullImpl adt opInfoPairs
   nullDecl <- qnullDecl

   -- Finally build the Signature declaration
   let n = return $ LitE $ StringL $ nameBase name
   let ops = [| M.fromList $(pairsToOpExprs opInfoPairs) |]
   let forcers = [| M.fromList $(readForcerObs adt opInfoPairs) |]
   let impls = listExpr $ buildImpl maybeShadowImpl maybeShadowExtractPair <$> implBuilders
   let nullImplementation = buildImpl maybeShadowImpl maybeShadowExtractPair nullInstanceBuilder
   let maybeShadow = buildImpl maybeShadowImpl maybeShadowExtractPair <$> maybeShadowBuilder
   let shadow =
         case maybeShadow of
            Nothing -> [| Nothing |]
            Just s  -> [| Just $s |]
   let sig = [| Signature $n $ops $forcers $impls $nullImplementation $shadow |]
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
   boundTyVarName <- nameBase <$> tyVarName v
   let tyfamilies = [n | (OpenTypeFamilyD (TypeFamilyHead n _ _ _)) <- sigds]
   let adt = ADTClass name boundTyVarName tyfamilies

   -- Collect the ADT methods
   (opInfoPairs, maybeShadowExtractPair) <- opPairsFromMethSigs adt name sigds

   -- collect all the instances of the class
   -- and extract out the implementations from the Shadow (if it exists)
   allImpls <- sequence $ mkImpl adt <$> insts
   let (maybeShadowImpl, implInsts) = select isShadowInst allImpls
   let implBuilders = attachMethods adt opInfoPairs <$> implInsts

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

opPairsFromMethSigs :: ADTDef -> Name -> [Dec] -> Q (Pairs, Maybe Pair)
opPairsFromMethSigs _ _ [] = return ([], Nothing)
-- can skip open type families that were collected earlier ...
opPairsFromMethSigs cls methName ((OpenTypeFamilyD _) : decls) =
   opPairsFromMethSigs cls methName decls
opPairsFromMethSigs cls _ ((SigD name ty) : decls) = do
   x <- opPairsFromMethSig cls name ty
   (xs, e) <- opPairsFromMethSigs cls name decls
   case (e, nameBase name == "extractShadow") of
      (es, False) -> return (x:xs, es)
      (Nothing, True)  -> return (xs, Just x)
      (Just _, True) -> fail $
         "Rufous: found multiple declarations of extractShadow."  -- Unreachable ?

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
argsFromType adt@(ADTClass _ tvar _) methName ty = do
      param_name <- get_param
      sequence $ uncurry ($) <$> zip (argFromType adt methName param_name <$> splitTys) tyFlags
   where
      tyFlags = replicate (n - 1) False ++ [True]
      n = length splitTys
      splitTys = splitTyArgs ty
      splitTyArgs (ForallT _ _ ty') = splitTyArgs ty'
      splitTyArgs (AppT (AppT ArrowT lhs) rhs) = lhs : splitTyArgs rhs
      splitTyArgs x = [x]
      adt_params (ForallT _ _ ty') = adt_params ty'
      adt_params (AppT (VarT v) (VarT c)) | nameBase v == tvar = [nameBase c]
      adt_params (AppT x y) = nub $ adt_params x ++ adt_params y
      adt_params _ = []
      get_param =
         case adt_params ty of
            [] -> fail $
               "Rufous: in method " ++ nameBase methName
               ++ " (:: " ++ pprint ty ++ ")"
               ++ ", found no use of container type variable " ++ tvar ++ "."
            [x] -> return x
            nms -> fail $
               "Rufous: in method " ++ nameBase methName
               ++ " (:: " ++ pprint ty ++ ")"
               ++ ", found multiple type variable aliases for contained type: "
               ++ intercalate "," nms ++ ";  but expected just one."

argFromType :: ADTDef -> Name -> String -> Type -> Bool -> Q ArgType
argFromType (ADTClass _ tvar tyfamilies) methName aty ty isRetTy = do
   let freeTvars = nameBase <$> tyVars ty

   -- class ADT t where
   case ty of
      --  (t a)  == Version
      AppT (VarT v) (VarT a) | nameBase v == tvar && nameBase a == aty -> return $ Version ()

      -- a       == VersionParam
      VarT v | nameBase v == aty  -> return $ NonVersion (VersionParam ())

      -- v      == NonVersion (undef :: v)
      t | tvar `notElem` freeTvars && aty `notElem` freeTvars ->
         return $ NonVersion (ArbArg () () (Just t))

      -- TyFamCtor t  == NonVersion (undef :: Int)
      -- ... we do this as there's no way to have the ADT t constraint floating safely
      (AppT (ConT c) (VarT v)) | c `elem` tyfamilies && nameBase v == tvar ->
         return $ NonVersion (ArbArg () () (Just (ConT ''Int)))
         -- (Just (ForallT [PlainTV v] [AppT (ConT clsName) (VarT v)] t))

      t | tvar `elem` freeTvars -> fail $
         "Rufous: in signature for method " ++ nameBase methName
         ++ " (:: " ++ pprint t ++ ")"
         ++ ", found occurrence of container type " ++ tvar
         ++ " outside the simple form (" ++ tvar ++ " " ++ aty ++ ")."

      -- -> f a  == NonVersion (undef :: f Int)
      -- ... for return types we do not need to generate them so they can stay loosely specified
      t | isRetTy ->
         return $ NonVersion (ArbArg () () (Just (concretize t)))

      t | aty `elem` freeTvars -> fail $
         "Rufous: in signature for method " ++ nameBase methName
         ++ " (:: " ++ pprint t ++ ")"
         ++ ", found occurrence of contained type " ++ aty
         ++ " outside the simple form (" ++ tvar ++ " " ++ aty ++ ")."

      x -> fail $
         "Rufous: non-simple type in signature for " ++ (nameBase methName)
         ++ ": " ++ (pprint x) ++ " cannot be interpreted as a simple type."

tyVars :: Type -> [Name]
tyVars (VarT v) = [v]
tyVars (AppT a b) = tyVars a ++ tyVars b
tyVars _ = []

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
       mkArg (NonVersion (ArbArg _ _ (Just t))) =
            let undef = return $ SigE (VarE 'undefined) t in
            [| NonVersion (ArbArg () $undef Nothing) |]
       mkArg (NonVersion (ArbArg _ _ Nothing)) = error "Rufous: unreachable."
   let mkArgs []     = [| [] |]
       mkArgs (x:xs) = [| $(mkArg x) : $(mkArgs xs) |]
   let (args', classify, retArg') = (mkArgs (init args), classifyArgs args, mkArg (last args))
   let mkClassifier' Mutator = [| Mutator |]
       mkClassifier' Generator = [| Generator |]
       mkClassifier' Observer = [| Observer |]
   [| ($var, Operation $var $retArg' $args' $(mkClassifier' classify)) |]


-- | given all the operations try find force* functions
-- e.g. given ADT t where view :: t a -> Maybe a
-- find forceView :: Maybe a -> IO ()
readForcerObs :: ADTDef -> Pairs -> Q Exp
readForcerObs cls ps = maybeListExpr $ pairToForceObs cls <$> ps

maybeListExpr :: [Q (Maybe Exp)] -> Q Exp
maybeListExpr [] = [| [] |]
maybeListExpr (e:es) = do
   r <- e
   case r of
      Just x  ->
         let e' = return x in
            [| $e' : ($(maybeListExpr es)) |]
      Nothing ->
         [|       ($(maybeListExpr es)) |]

pairToForceObs :: ADTDef -> (String, [ArgType]) -> Q (Maybe Exp)
pairToForceObs _ (methName, tys) = recover (return Nothing) comp
   where
      comp = do
         v <- reify (mkName ("force" ++ title methName))
         case v of
            VarI n _ _ -> do
               let dynCell = AppE (VarE 'toDyn) (SigE (VarE n) (concretize forceTy))
               let opNameLit = LitE (StringL methName)
               -- TH >= 2.16 makes these Maybe's ?
               return $ Just $ TupE [opNameLit, dynCell]
            _ -> fail "torecover"
      title (c:ws) = toUpper c : ws
      title [] = []
      forceTy = AppT (AppT ArrowT ret) (TupleT 0)
      ret = aTypeToType undefined (last tys)

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
data InstanceBuilder =
   Instance
      { instDef :: IMPLDef
      , instMethods :: [(String, Type, Type)]
      , instAbstMethods :: Pairs
      }


mkNullImpl :: ADTDef -> Pairs -> (InstanceBuilder, Q Dec)
mkNullImpl (ADTClass className _ familyCtors) pairs = (builder, q)
   where
      vars = mkImplBuilderVars (ConT ''Null) pairs
      inst = if null familyCtors then FirstOrder [] (ConT ''Null) else SecondOrder [] familyCtors (ConT ''Null)
      builder = Instance inst vars pairs
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

mkPats :: Bool -> [ArgType] -> [(Pat, Arg Name Name Name)]
mkPats readvars ats = go ats (0 :: Int)
   where
      go [] _ = []
      go (arg:args) k =
         let name' = mkName $ name k
         in (if readvars then VarP name' else WildP, mkArg arg name') : go args (k + 1)
      name k = "x" ++ show k
      mkArg (Version _) ident = Version ident
      mkArg (NonVersion (VersionParam _)) ident = NonVersion (VersionParam ident)
      mkArg (NonVersion (ArbArg _ a b)) ident = NonVersion (ArbArg ident a b)

mkExtractorImpls :: ADTDef -> Pairs -> [InstanceBuilder] -> Q [Dec]
mkExtractorImpls cls pairs impls = sequence . map (mkExtractorImpl cls pairs) $ impls

extractorTy :: ADTDef -> Type -> Q Type
extractorTy (ADTClass nm _ _) ty = return $ AppT (ConT nm) (AppT (ConT ''WrappedADT) (concretize ty))

mkExtractorImpl :: ADTDef -> Pairs -> InstanceBuilder -> Q Dec
mkExtractorImpl cls pairs ib =
   case instDef ib of
      FirstOrder _ ty -> do
         ds <- mkExtractorImplFromPairs pairs
         ety <- extractorTy cls ty
         return $ InstanceD Nothing [] ety ds
      SecondOrder _ families ty -> do
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

buildCall :: Name -> [(Integer, (Pat, Arg Name Name Name))] -> Q Exp -> Q Exp
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
            [| nonversion $curId $ivar (NonVersion (VersionParam $var)) $var |]
      mkArg i (NonVersion (ArbArg v _ (Just _))) =
            let var = return $ VarE $ v in
            let ivar = return $ LitE $ IntegerL i in
            [| nonversion $curId $ivar (NonVersion (ArbArg () $var Nothing)) $var |]
      mkArg _ (NonVersion (ArbArg _ _ _)) = error "Rufous: unreachable."

isShadowImpl :: InstanceBuilder -> Bool
isShadowImpl i = isShadowInst (instDef i)

isShadowInst :: IMPLDef -> Bool
isShadowInst t =
   case t of
      FirstOrder _ ty -> go ty
      SecondOrder _ _ ty -> go ty
   where
      go ty =
         case ty of
            ConT name -> "Shadow" `isPrefixOf` nameBase name
            AppT a _ -> go a
            _ -> False

mkImpl :: ADTDef -> Dec -> Q IMPLDef
mkImpl cls (InstanceD Nothing cxts (AppT (ConT _) c) _) = return $ implF cxts c
   where implF cxts =
            case adtTyFamilyCtors cls of
               [] -> FirstOrder cxts
               tys -> SecondOrder  cxts tys
mkImpl _ i@(InstanceD _ _ _ _) = fail $
   "Rufous: unsupported instance declaration at " ++ prettyShowDec i
mkImpl _ d = fail $
   "Rufous: expected instance declaration, not " ++ prettyShowDec d

attachMethods :: ADTDef -> Pairs -> IMPLDef -> InstanceBuilder
attachMethods _ pairs impl = Instance impl (mkImplBuilderVars (implTyCtor impl) pairs) pairs

mkImplBuilderVars :: Type -> Pairs -> [(String, Type, Type)]
mkImplBuilderVars _ [] = []
mkImplBuilderVars ty (p:ps) = mkImplBuilderVar ty p : mkImplBuilderVars ty ps

mkImplBuilderVar :: Type -> Pair -> (String, Type, Type)
mkImplBuilderVar ty (name, args) = (name, argTysToType ty args, aTypeToType ty finalArg)
   where
      finalArg = last args

buildImpl :: Maybe IMPLDef -> Maybe Pair -> InstanceBuilder -> Q Exp
-- TODO: Cxt ?  e.g. what to do about `instance Ord k => A (Foo k)` ?
buildImpl maybeShadow maybeShadowExtractor i = [| Implementation $(ctorNameStr) (M.fromList $methods) $maybeShadowExtractorExp $eqTysExpr $showTysExpr |]
   where ctorNameStr = return $ LitE $ StringL (userfriendlyTypeString (implTyCtor (instDef i)))
         methods = buildImplPairs (instMethods i)
         maybeShadowExtractorExp =
            case (maybeShadow, maybeShadowExtractor) of
               (_, Nothing) -> [| Nothing |]
               (Just sh, Just shExt) -> [| Just $(shadowMethodDyn sh shExt i) |]
               (Nothing, Just _) -> fail $
                  "Rufous: cannot generate shadow extractors without a shadow definition."

         showTysExpr = [| Just (M.fromList ($(listExpr $ tcDictPairs implShowDyn (instDef i)))) |] -- always create Show instances

         eqTysExpr =
            case (isShadowImpl i, maybeShadowExtractor, maybeShadow) of
               (False, _, _) -> [| Nothing |]
               (True, Just _, Just sh) -> [| Just (M.fromList ($(listExpr $ tcDictPairs implEqDyn sh))) |]
               (True, Nothing, _) -> [| Nothing |]
               (True, Just _, Nothing) -> [| Nothing |]

         tcDictPairs g sh = map (tcDictPair g sh) (instAbstMethods i)
         tcDictPair g sh m@(n, _) = [| ($(return $ LitE $ StringL n), $(g sh m)) |]

type TCDictGen = IMPLDef -> Pair -> Q Exp

-- | Generate the expression (toDyn ((==) :: t))
-- where t is determined solely by the operation being used:
-- for generators and mutators, t == (ImplTy Int -> ImplTy Int -> Bool)
-- for observers of return type r then t == (r -> r -> int)
implEqDyn :: TCDictGen
implEqDyn = implTCDictDyn '(==) eqTy
   where eqTy mRetTy = (AppT (AppT ArrowT mRetTy)
                      (AppT (AppT ArrowT mRetTy)
                            (ConT ''Bool)))

implShowDyn :: TCDictGen
implShowDyn = implTCDictDyn 'show showTy
   where showTy mRetTy = (AppT (AppT ArrowT mRetTy)
                               (ConT ''String))

implTCDictDyn :: Name -> (Type -> Type) -> TCDictGen
implTCDictDyn nm tyBuilder impl (_, tys) = [| toDyn $methE |]
   where methE = return $ SigE (VarE nm) methTy
         methTy = tyBuilder mRetTy
         implTy = concretize $ (AppT implCtorTy (ConT ''Int))
         implCtorTy = implTyCtor impl
         mRetArgTy = last tys
         mRetTy =
            case mRetArgTy of
               Version _ -> implTy
               NonVersion (VersionParam _) -> ConT ''Int
               NonVersion (ArbArg _ _ (Just t)) -> t
               NonVersion (ArbArg _ _ Nothing) -> error "unreachable"

shadowMethodDyn :: IMPLDef -> Pair -> InstanceBuilder -> Q Exp
shadowMethodDyn shadow (name,_) implIb = [| toDyn $shadowTerm |]
   where shadowTerm = return $ SigE (VarE (mkName name)) concreteExtractTy
         shadowCtorTy = implTyCtor shadow
         instTy = implTyCtor (instDef implIb)
         extractTy = AppT (AppT ArrowT
                              (AppT instTy (ConT ''Int)))
                              (AppT shadowCtorTy (ConT ''Int))
         concreteExtractTy = concretize extractTy


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
-- t a => t Int
aTypeToType ty (Version ()) = AppT (concretize ty) (ConT (mkName "Int"))
aTypeToType _ (NonVersion (VersionParam _)) = ConT (mkName "Int")
aTypeToType _ (NonVersion (ArbArg _ _ (Just t))) = t
aTypeToType _ (NonVersion (ArbArg _ _ Nothing)) = error "Rufous: unreachable."

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