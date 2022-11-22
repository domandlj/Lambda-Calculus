{-# LANGUAGE UnicodeSyntax #-}
module Macros (
 Namespace,
 applyMacros,
 getMacro,
 macros,
 getCoincidence
) where

import Data.Text (replace, pack, unpack) 
import Data.String 
import Data.List
import Text.ParserCombinators.Parsec
import Ast
import Eval (eval)
import Parser (parseLC)

type Namespace = [(String, String)]


applyMacros :: String -> Namespace -> String 
applyMacros str [] = str
applyMacros str ((name, sub):xs) =
  let 
    str' = intercalate " " $ map (\word-> 
      if word == name then 
        "("++ sub ++ ")"
      else word) $ words str  
  in
    applyMacros str' xs




macrosToPTerm xs = [ (name, parseLC sub) | (name, sub) <- xs]

macrosToTerm xs = [(name, tryDeleteNames t) | (name, t) <- xs]
  where tryDeleteNames (Left e ) = Left e
        tryDeleteNames (Right s) = Right $ deleteNames [] s

macrosToEvalTerms xs = [(name, eval [] t) | (name, t) <- xs]

macrosTerm [] = []
macrosTerm ((name,value):xs) = case value of
  Left _ -> macrosTerm xs
  Right t -> ((name, t):macrosTerm xs)

getCoincidence :: Term -> [(String, String)] -> [(String, Term)]       
getCoincidence t macros = 
  let 
    ms = macrosTerm $ macrosTerm $ macrosToTerm $ macrosToPTerm macros
    ms' = macrosToEvalTerms ms
  in
    filter (\(name, t') -> t' == t) ms'
    

getMacro :: String -> Maybe (String, String)
getMacro str = 
  let 
    tokens = words str
  in
    if length tokens < 4 then
      Nothing
    else
      Just (tokens !! 1, intercalate " " $ drop 3 tokens)



-- data
macros :: Namespace
macros = [
  ("id", "(λx.x)"),
  ("false", "(λx.λy.y)"),
  ("true", "(λx.λy.x)"),
  ("and", "(λp.λq.((p q) (λx.λy.y) ))"),
  ("if", "(λb.λx.λy.((b x) y))"),
  ("or", "(λp.λq.((( (λb.λx.λy.((b x) y)) p) (λx.λy.x) ) q))"),
  ("pair", "(λf.λs.λb.b f s)"),
  ("fst", "(λf.f (λx.λy.x))"),
  ("snd", "(λf.f (λx.λy.y))"),
  ("zero", "(λs.λz.z)"),
  ("suc", "(λn.λs.λz.s (n s z))"),
  ("iszero", "(λf.f (λx.(λp.λq.q)) (λp.λq.p))"),
  ("plus", "(λm.λn.n (λn.λs.λz.s (n s z)) m)")]
