{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.TH where

import qualified Data.Map as M

import Data.List (isPrefixOf)

import Data.Dynamic (toDyn)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (showName)

import Test.Rufous.Signature

import Control.Exception
import Test.Rufous.Exceptions

-- Classifying a list of args into a type is straightforward:

isVersion (Version v) = True
isVersion _ = False

classifyArgs :: [ArgType] -> OperationType
classifyArgs args =
   if not . isVersion $ last args then
      Observer
   else
      if any isVersion (init args) then
         Mutator
      else
         Generator

-- makeRufousSpec will extract information about a given class
makeRufousSpec :: Name -> DecsQ
makeRufousSpec name = do
   info <- reify name
   case info of
      ClassI (ClassD ctx name tys funds sigds) insts -> do
         let tyPairs = pairsFromSigds sigds
         let opPairs = pairsToQ tyPairs
         let ops = [| M.fromList $opPairs |]
         let (maybeShadow, implBuilders) = selectBuilders $ mkImplBuilders tyPairs insts
         let impls = buildImpls implBuilders
         let (nullImpl, qnullDecl) = mkNullImpl name tyPairs
         nullDecl <- qnullDecl
         let shadow = case maybeShadow of
               Nothing      -> [| Nothing |]
               Just builder -> [| Just $(buildImpl builder) |]

         shadowTy <- case maybeShadow of
               Nothing      -> VarT <$> newName "a"
               Just (ty, _) -> return $ AppT ty (TupleT 0)


         let sig = [| Signature $ops $impls (Just $(buildImpl nullImpl)) $shadow  |]
         let specName = mkName $ "_" ++ (nameBase name)
         let specPat = return $ VarP specName
         ds <- [d| $specPat = $sig |]
         return $ nullDecl : ds
      _ -> fail "makeRufousSpec expected class name as argument"

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
      VarT name -> NonVersion (showName name)
      x -> error $ "argFromType :: Unexpected value " ++ show x

pairsToQ :: Pairs -> Q Exp
pairsToQ [] = [| [] |]
pairsToQ (pair:pairs) = [| $(pairToQ pair) : $(pairsToQ pairs) |]

pairToQ :: (String, [ArgType]) -> Q Exp
pairToQ (name, args) = do 
   let var = return $ LitE $ StringL name
   let mkArg (Version ()) = [| Version () |]
       mkArg (NonVersion x) = [| NonVersion $(return $ LitE $ StringL x) |]
   let mkArgs []     = [| [] |] 
       mkArgs (x:xs) = [| $(mkArg x) : $(mkArgs xs) |]
   let (args', classify, retArg') = (mkArgs (init args), classifyArgs args, mkArg (last args))
   let mkClassifier' Mutator = [| Mutator |]
       mkClassifier' Generator = [| Generator |]
       mkClassifier' Observer = [| Observer |]
   [| ($var, Operation $var (OperationSig $args' $(mkClassifier' classify) $retArg')) |]

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
   let patterns = mkPats (init args)
   if isVersion . last $ args then do
      null' <- [| NullImpl |]
      return $ [FunD name' [Clause patterns (NormalB $ null') []]]
   else do
      impl <- [| throw NotImplemented |]
      return  $ [FunD name' [Clause patterns (NormalB $ impl) []]]

mkPats :: [ArgType] -> [Pat]
mkPats [] = []
mkPats (arg:args) = WildP : mkPats args


isShadowImpl :: InstanceBuilder -> Bool
isShadowImpl (ty, _) = 
   case ty of
      ConT name -> "Shadow" `isPrefixOf` nameBase name
      x         -> False

mkImplBuilders :: Pairs -> [InstanceDec] -> [InstanceBuilder]
mkImplBuilders pairs [] = []
mkImplBuilders pairs (inst:insts) = mkImplBuilder pairs inst : mkImplBuilders pairs insts

mkImplBuilder :: Pairs -> InstanceDec -> InstanceBuilder
mkImplBuilder pairs (InstanceD Nothing _ (AppT _ ty) []) =
   (ty, mkImplBuilderVars ty pairs)

mkImplBuilderVars :: Type -> Pairs -> [(String, Type, Type)]
mkImplBuilderVars ty [] = [] 
mkImplBuilderVars ty (p:ps) = mkImplBuilderVar ty p : mkImplBuilderVars ty ps

mkImplBuilderVar :: Type -> Pair -> (String, Type, Type)
mkImplBuilderVar ty (name, args) = (name, argTys2Type ty args, aType2Type ty finalArg)
   where
      finalArg = last args

buildImpls :: [InstanceBuilder] -> Q Exp
buildImpls [] = [| [] |]
buildImpls (impl:impls) = [| $(buildImpl impl) : $(buildImpls impls) |]

buildImpl :: InstanceBuilder -> Q Exp
buildImpl (ctorTy, pairs) = [| Implementation (M.fromList $(buildImplPairs pairs)) |]

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
argTys2Type :: Type -> [ArgType] -> Type
argTys2Type ty [retTy]   = aType2Type ty retTy
argTys2Type ty (aty:tys) = AppT (AppT ArrowT (aType2Type ty aty)) (argTys2Type ty tys)

aType2Type :: Type -> ArgType -> Type
aType2Type ty (Version ()) = AppT ty (ConT (mkName "Int"))
aType2Type ty (NonVersion x) = ConT (mkName "Int")
