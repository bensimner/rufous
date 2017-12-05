module Test.Rufous.SigParser
   ( OperationType(..)
   , Arg(..)
   , OperationSig(..)
   , parseSig
   ) where

import Text.Parsec
import Control.Applicative ((<$), (<*), (*>), liftA)

data OperationType = Mutator | Observer | Generator
   deriving (Show)

data Arg = Version | NonVersion
   deriving (Eq, Show)

data OperationSig =
   OperationSig
      { opArgs :: [Arg]
      , opType :: OperationType
      }
   deriving (Show)

-- this is slightly hardcoded for now.
parseSig :: String -> OperationSig
parseSig s =
   case parse parseSignature "<string>" s of
      Right op -> op
      Left  e  -> error (show e)

classifyArgs :: [Arg] -> OperationType
classifyArgs args =
   if last args /= Version then
      Observer
   else
      if Version `elem` init args then
         Mutator
      else
         Generator

parseSignature :: Parsec String () OperationSig
parseSignature = (\as -> OperationSig as (classifyArgs as)) <$> (sepBy parseArg parseArrow <* eof)

parseArrow :: Parsec String () String
parseArrow = many space *> string "->" <* many space

parseVersion :: Parsec String () Arg
parseVersion = const Version <$> (char 'T' *> space *> char 'a')

parseNonVersion :: Parsec String () Arg
parseNonVersion = const NonVersion <$> char 'a'

parseArg :: Parsec String () Arg
parseArg = parseVersion <|> parseNonVersion