{-# LANGUAGE UnicodeSyntax #-}
module Ast (
  PTerm(PVar, PAbs, PApp),
  Term(Var, Abs, App),
  Context,
  deleteNames,
  restoreNames,
  printPTerm,
  printTerm
) where
import Data.String
import Data.List


data PTerm =
    PVar String        -- (PVar x) ≈  x
  | PAbs String PTerm  -- (PAbs x t) ≈ λx.t
  | PApp PTerm PTerm   -- (PApp t s)  ≈ t s
  deriving Show


data Term =
    Var Int         -- (Var n) ≈  n
  | Abs Term        -- (Abs t) ≈ (λ . t)
  | App Term Term   -- (App t s) ≈ t s
  deriving (Show, Eq)



printPTerm :: PTerm -> String
printPTerm (PVar x) = x
printPTerm (PAbs x t) = "(λ" ++ x ++ "." ++ printPTerm t ++ ")"
printPTerm (PApp t s) = printPTerm t ++ " " ++ printPTerm s


printTerm :: Term -> String
printTerm (Var x) = show x
printTerm (Abs t) = "(λ." ++ printTerm t ++ ")"
printTerm (App t s) = printTerm t ++ " " ++ printTerm s


type Context = [String]

getIndex :: Context -> String -> Int
getIndex ctx x =
  let
    ctx' = reverse ctx
    index = findIndex (==x) ctx'
  in
    case index of
      Nothing -> -1
      Just i  -> i

getName :: Context -> Int -> String
getName ctx x =
  let
    ctx' = reverse ctx
  in
    ctx' !! x




deleteNames :: Context -> PTerm -> Either String Term
deleteNames ctx (PVar x)
  | getIndex ctx x /= -1 = Right $ Var $ getIndex ctx x
  | otherwise           = Left "😡 Only closed terms!"
deleteNames ctx (PAbs x t) = do
  a <- deleteNames (ctx ++ [x]) t
  return $ Abs a

deleteNames ctx (PApp t s) = do 
  a  <- (deleteNames ctx t) 
  b  <- (deleteNames ctx s)
  return $ App a b


restoreNames :: Context -> Term -> PTerm
restoreNames ctx (Var x) = PVar (getName ctx x)
restoreNames ctx (App t s) = PApp (restoreNames ctx t) (restoreNames ctx s)
restoreNames ctx (Abs t) = PAbs fresh (restoreNames ctx' t)
        where fresh = head [ var | var <- vars, not (elem var ctx)]
              vars  = ["x" ++ (show i) | i <- [0 .. ] ]
              ctx' = ctx ++ [fresh]
