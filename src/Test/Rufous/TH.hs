{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.TH where

import qualified Data.Map as M

import Language.Haskell.TH

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
         let pairs = [| $(mkOps sigds) |]
         let ops = [| M.fromList $pairs |]
         let impls = [| [] |]
         let sig = [| Signature $ops $impls |]
         [d| rufousSpec = $sig |]
      _ -> fail "makeRufousSpec expected class name as argument"

mkOps :: [Dec] -> Q Exp
mkOps [] = [| [] |]
mkOps (d : ds) = [| $(mkOp d) : $(mkOps ds) |]

mkOp :: Dec -> Q Exp
mkOp (SigD name ty) = do
   let var = return $ LitE $ StringL (nameBase name)
   [| ($var, Operation $var $(mkOpSig ty)) |]

mkOpSig :: Type -> Q Exp
mkOpSig ty = [| let args = $(mkOpArgs ty) in OperationSig (init args) (classifyArgs args) (last args) |]

mkOpArgs :: Type -> Q Exp
mkOpArgs ty = 
   case ty of
      ForallT _ _ ty -> mkOpArgs ty  -- unwrap the forall on the class constraint
      AppT (AppT ArrowT lhs) rhs -> [| $(mkOpArg lhs) : $(mkOpArgs rhs) |]
      x                          -> [| [ $(mkOpArg x) ] |]

mkOpArg :: Type -> Q Exp
mkOpArg ty =
   case ty of
      AppT (VarT ctorName) (VarT varName) -> [| Version () |]
      VarT name -> do
         let nameStr = return $ LitE $ StringL (nameBase name)
         [| NonVersion $nameStr |]
      x -> fail $ "mkOpArg :: Unexpected case :: " ++ show x
