{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.TH where

import qualified Data.Map as M

import Debug.Trace

import Data.Dynamic (toDyn)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (showName)

import Test.Rufous.Signature

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
         let impls = traceShow ("mkImpls", insts, tyPairs) (mkImpls tyPairs insts)
         let sig = [| Signature $ops $impls |]
         let specName = return $ VarP $ mkName $ "_" ++ (nameBase name)
         [d| $specName = $sig |]
      _ -> fail "makeRufousSpec expected class name as argument"

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

mkImpls :: Pairs -> [InstanceDec] -> Q Exp
mkImpls pairs [] = [| [] |]
mkImpls pairs (decl:decls) = [| $(mkImpl pairs decl) : $(mkImpls pairs decls) |]

mkImpl :: Pairs -> InstanceDec -> Q Exp
mkImpl pairs (InstanceD Nothing _ (AppT _ ty) []) = 
   traceShow ("mkImpl", ty, pairs) [| M.fromList $(mkImplPairs ty pairs) |]
mkImpl _ x = error $ "mkImpl :: Got Unexpected instance declaration :: " ++ show x

mkImplPairs :: Type -> Pairs -> Q Exp
mkImplPairs ty [] = [| [] |]
mkImplPairs ty (pair : pairs) = [| $(mkImplPair ty pair) : $(mkImplPairs ty pairs) |]

mkImplPair :: Type -> Pair -> Q Exp
mkImplPair ty (name, args) = [| ($nameStr, $var) |] 
   where
      nameStr = return $ LitE $ StringL name
      var = return $ AppE (VarE 'toDyn) (SigE (VarE (mkName name)) (argTys2Type ty args))

-- Convert Version/NonVersion arguments to proper (concrete) Type expressions
-- todo: This needs to be more principled, this translation turns everything not-concrete into `Int'
-- which isn't quite right ...
argTys2Type :: Type -> [ArgType] -> Type
argTys2Type ty [retTy]   = aType2Type ty retTy
argTys2Type ty (aty:tys) = AppT (AppT ArrowT (aType2Type ty aty)) (argTys2Type ty tys)

aType2Type :: Type -> ArgType -> Type
aType2Type ty (Version ()) = AppT ty (ConT (mkName "Int"))
aType2Type ty (NonVersion x) = ConT (mkName "Int")
