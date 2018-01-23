> {-# LANGUAGE TemplateHaskell #-}
> module Test.Rufous.SigConstructor where
> 
> import Control.Lens
>
> import qualified Data.Map as M
>
> import Text.Parsec (Parsec, parse, many, string, space, char, sepBy, eof, alphaNum)
> import Control.Applicative ((<*), (*>), (<|>))
> import Language.Haskell.Exts (parseFile, fromParseResult, SrcSpanInfo, ParseResult(..))
> import Language.Haskell.Exts.SrcLoc
> import Language.Haskell.Exts.Syntax (Module(..), Decl(..), ClassDecl(..), DeclHead(..) 
>                                     , FunDep(..), Name(..), TyVarBind(..), Type(..)
>                                     , QName(..))
> 
> import Test.Rufous.Signature


Classifying a list of args into a type is straightforward:

> isVersion (Version v) = True
> isVersion _ = False

> classifyArgs :: [ArgType] -> OperationType
> classifyArgs args =
>    if not . isVersion $ last args then
>       Observer
>    else
>       if any isVersion (init args) then
>          Mutator
>       else
>          Generator

Using haskell-src-exts to parse the module file to create a signature from the class:

We do this by constructing a partial Signature type as we go:

> data OperationBuilder =
>   OperationBuilder
>       { _opBuildName :: String
>       , _opBuildTys  :: [ArgType]
>       }
>   deriving (Show)
> makeLenses ''OperationBuilder
>
> data SignatureBuilder =
>   SignatureBuilder 
>       { _sigTyName :: String
>       , _sigOpers  :: [OperationBuilder]
>       , _sigImpls  :: [String]
>       }
>   deriving (Show)
> makeLenses ''SignatureBuilder
> 
> data Failure = Failure SrcSpanInfo String
>   deriving (Show)
> type SignatureResult = Either Failure SignatureBuilder

> readModule :: Module SrcSpanInfo -> SignatureResult
> readModule (Module srcspan modHead _ _ modDecls) = readDecls srcspan modDecls
>
> readDecls :: SrcSpanInfo -> [Decl SrcSpanInfo] -> SignatureResult
> readDecls src decls = go decls (SignatureBuilder "" [] [])
>   where
>       go :: [Decl SrcSpanInfo] -> SignatureBuilder -> SignatureResult
>       go [] s = return s
>       go (d : ds) x = do
>           case d of 
>               ClassDecl srcSpan _ declHead _ (Just decls) -> do
>                   x' <- readClass srcSpan declHead decls x
>                   go ds x'
>               _ -> go ds x -- todo: remember operations

>           
> readClass :: SrcSpanInfo -> DeclHead SrcSpanInfo -> [ClassDecl SrcSpanInfo] -> SignatureBuilder -> SignatureResult
> readClass src declHead fds s = do
>       s' <- addTyName s
>       s'' <- addOperations fds s'
>       return s''
>   where
>       addTyName s = 
>           case declHead of
>               -- "class className tyVar where ..."
>               DHApp _ (DHead _ (Ident _ className)) (UnkindedVar _ (Ident _ tyVar))  ->
>                   Right $ s & sigTyName .~ tyVar
>               x -> Left $ Failure src ("Invalid class declaration header :: " ++ show x)
>       addOperations (decl:decls) s = do
>           opBuilder <- readOperation (s ^. sigTyName) src decl
>           return $ s & sigOpers %~ (++ [opBuilder]) 
>
> readOperation :: String -> SrcSpanInfo -> ClassDecl SrcSpanInfo -> Either Failure OperationBuilder
> readOperation ctorName src (ClsDecl _ d) =
>   case d of
>       TypeSig _ [Ident _ name] ty -> do
>           tys <- readTy src ctorName ty
>           return $ OperationBuilder name tys
>       x -> Left $ Failure src ("Invalid operation signature :: " ++ show x)

> readTy :: SrcSpanInfo -> String -> Type SrcSpanInfo -> Either Failure [ArgType]
> readTy tySrc ctorName ty =
>   case ty of
>       -- simple types can only have non-function types on the lhs
>       TyFun src lhs rhs -> do
>           lhsTy <- readArgTy src ctorName lhs
>           args' <- readTy src ctorName rhs
>           return $ lhsTy : args' 
>       x -> do
>           r <- readArgTy tySrc ctorName x
>           return [r]
>           
> readArgTy :: SrcSpanInfo -> String -> Type SrcSpanInfo -> Either Failure ArgType
> readArgTy argSrc ctorName t =
>   case t of
>       TyFun src _ _ -> Left $ Failure src ("Invalid type signature :: higher-order types not allowed")
>       TyCon _ (UnQual _ (Ident _ name)) -> return $ NonVersion name
>       TyVar _ (Ident _ var) -> return $ NonVersion var
>       TyApp src (TyVar _ (Ident _ name)) (TyVar _ (Ident _ var)) -> 
>           if name == ctorName then
>               -- todo: use the type variable
>               return $ Version ()
>           else
>               Left $ Failure src ("Invalid type signature :: Non-version type application not allowed.")
>       x -> Left $ Failure argSrc ("Invalid type signature :: unknown construct :: " ++ show x)
>   

Finally the last thing to do is read the SignatureReuslt, unwrap the SignatureBuilder
and use it to build the Signature

> buildSig :: [ArgType] -> OperationSig
> buildSig tys = 
>   OperationSig (init tys) (classifyArgs tys) (last tys)
>
> buildOperation :: OperationBuilder -> Operation
> buildOperation o = 
>   Operation (o ^. opBuildName) (buildSig (o ^. opBuildTys))
> 
> buildSignature :: SignatureBuilder -> Signature
> buildSignature s =
>   Signature (M.fromList [(o ^. opName, o) | o <- map buildOperation (s ^. sigOpers)]) []

> readModuleFile :: String -> IO (Either Failure Signature)
> readModuleFile path = do
>   parseResult <- parseFile path
>   case parseResult of
>       ParseOk moduleSyn -> return $ do
>           builder <- readModule moduleSyn
>           return $ buildSignature builder
>       ParseFailed loc msg -> return $ Left $ Failure srcSpanInfo msg
>           where
>               srcSpan =  mkSrcSpan loc loc
>               srcSpanInfo = noInfoSpan srcSpan
