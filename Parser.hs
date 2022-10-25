{-# LANGUAGE UnicodeSyntax #-}

-- https://jakewheat.github.io/intro_to_parsing/#combinator-review
module Parser (parseLC) where

import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Safe
import Ast (PTerm(PVar, PAbs, PApp))

type Name = String

type LCParser = GenParser Char () PTerm

varName :: GenParser Char () Name
varName = many1 $ letter

var :: LCParser
var = PVar <$> varName

abstr :: LCParser -> LCParser
abstr expr = do
    spaces
    char 'λ'
    spaces
    name <- varName
    char '.'
    spaces
    body <- expr
    spaces
    return $ PAbs name body
   
parens :: LCParser
parens = do
  spaces
  char '('
  spaces
  term <- expr
  char ')'
  spaces
  return term


nonApp :: LCParser
nonApp = parens <|> abstr expr <|> var 
    
expr :: LCParser
expr = chainl1 nonApp $ optional space >> return PApp
    
parseLC :: String -> Either ParseError PTerm
parseLC = parse expr ""

