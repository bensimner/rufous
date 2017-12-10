> module Test.Rufous.SigParser
>    ( OperationType(..)
>    , Arg(..)
>    , OperationSig(..)
>    , parseSig
>    , sig2str
>    ) where
> 
> import Data.List (intercalate)
> import Text.Parsec (Parsec, parse, many, string, space, char, sepBy, eof)
> import Control.Applicative ((<*), (*>), (<|>))

Operations are functions whose signatures conform to a simple grammar
These signatures are essentially combinations of Version / NonVersion first-order arguments

> data Arg = Version | NonVersion
>    deriving (Eq, Show)

Each operation falls into 1 of 3 categories, depending on the signature. 
    - Generators take NonVersion arguments and return Version's
    - Observers return NonVersion
    - Mutator's return Versions

> data OperationType = Mutator | Observer | Generator
>    deriving (Eq, Show)
> 
> classifyArgs :: [Arg] -> OperationType
> classifyArgs args =
>    if last args /= Version then
>       Observer
>    else
>       if Version `elem` init args then
>          Mutator
>       else
>          Generator

Hence an operation's signature is simply a pair, the function signature and its categorical type:

> data OperationSig =
>    OperationSig
>       { opArgs :: [Arg]
>       , opType :: OperationType
>       , retTy :: Arg
>       }
>    deriving (Eq, Show)
> 
> sig2str :: OperationSig -> String
> sig2str s = intercalate " -> " $ [arg2str a | a <- opArgs s] ++ [arg2str $ retTy s]
> arg2str :: Arg -> String
> arg2str a =
>   case a of
>       Version -> "t a"
>       NonVersion -> "a"

These signatures are built by a simple parser with the grammar:
    <sig> ::= <arg> | <arg> "->" <sig>
    <arg> ::= <version> | <nonversion>
    <version>    ::= "T a"
    <nonversion> ::= "a"

The parser is a straight-forward parser built from combinators for each production in the grammar.

> parseSig :: String -> OperationSig
> parseSig s =
>    case parse parseSignature "<string>" s of
>       Right op -> op
>       Left  e  -> error (show e)
> 
> 
> parseSignature :: Parsec String () OperationSig
> parseSignature = (\as -> OperationSig (init as) (classifyArgs as) (last as)) <$> (sepBy parseArg parseArrow <* eof)
> 
> parseArrow :: Parsec String () String
> parseArrow = many space *> string "->" <* many space
> 
> parseVersion :: Parsec String () Arg
> parseVersion = const Version <$> (char 'T' *> space *> char 'a')
> 
> parseNonVersion :: Parsec String () Arg
> parseNonVersion = const NonVersion <$> char 'a'
> 
> parseArg :: Parsec String () Arg
> parseArg = parseVersion <|> parseNonVersion
