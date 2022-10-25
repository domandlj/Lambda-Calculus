{-# LANGUAGE UnicodeSyntax #-}
module Eval (eval) where
import Ast

shift :: Int -> Int -> Term -> Term
shift i c (Var x)
  | x < c       = Var x 
  | otherwise   = Var (x + i) 
shift i c (Abs t) = Abs (shift i (c+1) t)
shift i c (App t s) = App (shift i c t) (shift i c s)

-- [j -> s]t  
subst :: Int -> Term -> Term -> Term
subst j s (Var x)
  | j == x    = s 
  | otherwise = Var x
subst j s (Abs t) = Abs $ subst (j+1) (shift 1 0 s) t
subst j s (App t1 t2) = App (subst j s t1) (subst j s t2)


-- Call by Value evaluation.
isVal :: Term -> Bool
isVal (Abs _ ) = True
isVal _ = False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx (App v@(Abs t1) t2)
  | isVal t2 = 
      let 
        beta = shift (-1) 0 $ subst 0 (shift 1 0 t2) t1
      in 
        Just $ beta
  | otherwise = do {
      t2' <- eval1 ctx t2; 
      return (App v t2')
    }

eval1 ctx (App t1 t2) = do {
    t1' <- eval1 ctx t1; 
    return (App t1' t2)
  }
   
eval1 ctx _ = Nothing

eval ctx t = case eval1 ctx t of
  Just t' -> eval ctx t'
  Nothing -> t
