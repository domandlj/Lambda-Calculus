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
        sub 
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
  ("ID", "(λx.x)"),
  ("FALSE", "(λx.λy.y)"),
  ("TRUE", "(λx.λy.x)"),
  ("AND", "(λp.λq.((p q) (λx.λy.y) ))"),
  ("IF", "(λb.λx.λy.((b x) y))"),
  ("OR", "(λp.λq.((( (λb.λx.λy.((b x) y)) p) (λx.λy.x) ) q))"),
  ("PAIR", "(λf.λs.λb.b f s)"),
  ("FST", "(λf.f (λx.λy.x))"),
  ("SND", "(λf.f (λx.λy.y))"),
  ("ZERO", "(λs.λz.z)"),
  ("SUCC", "(λn.λs.λz.s (n s z))"),
  ("ISZERO", "(λf.f (λx.(λp.λq.q)) (λp.λq.p))"),
  ("PLUS", "(λm.λn.n (λn.λs.λz.s (n s z)) m)")]
