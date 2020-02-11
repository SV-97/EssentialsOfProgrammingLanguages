module Exp where

import           Control.Monad.State
import           Env
import           ExpVal              (ExpVal)
import qualified ExpVal
import           LET

data Exp
  = ConstExp Int
  | DiffExp Exp Exp
  | VarExp Var
  | IsZeroExp Exp
  | IfExp Exp Exp Exp
  | LetExp Var Exp Exp
  | EmptyListExp
  | ConsExp Exp Exp
  | HeadExp Exp
  | TailExp Exp
  | IsEmptyExp Exp

valueOf :: Exp -> State Env (Maybe ExpVal)
valueOf (ConstExp n) = return . Just $ ExpVal.Num n
valueOf (VarExp var) = apply var
valueOf (DiffExp exp1 exp2) = do
  x1 <- valueOf exp1
  x2 <- valueOf exp2
  return $ do
    v1 <- x1 >>= ExpVal.toNum
    v2 <- x2 >>= ExpVal.toNum
    return $ ExpVal.Num (v1 - v2)
valueOf (IsZeroExp exp) = do
  x1 <- valueOf exp
  return $ ExpVal.Bool <$> ((==) <$> x1 <*> pure (ExpVal.Num 0))
valueOf (IfExp exp1 exp2 exp3) = do
  val1 <- valueOf exp1
  case val1 of
    Just (ExpVal.Bool True)  -> valueOf exp2
    Just (ExpVal.Bool False) -> valueOf exp3
    _                        -> return Nothing
valueOf (LetExp var exp1 body) = do
  ρ1 <- get
  val1 <- valueOf exp1
  case val1 of
    Just v -> do
      extend var v
      val <- valueOf body
      put ρ1
      return val
    Nothing -> return Nothing
valueOf (ConsExp exp1 exp2) = do
  x1 <- valueOf exp1
  x2 <- valueOf exp2
  return $ do
    v1 <- x1
    v2 <- x2 >>= ExpVal.toList
    return $ ExpVal.List (v1 : v2)
valueOf (HeadExp exp1) = do
  x1 <- valueOf exp1
  return $ do
    v1 <- x1 >>= ExpVal.toList
    return $ head v1
valueOf (TailExp exp1) = do
  x1 <- valueOf exp1
  return $ do
    v1 <- x1 >>= ExpVal.toList
    return $ ExpVal.List (tail v1)
valueOf EmptyListExp = return . Just $ ExpVal.List []
valueOf (IsEmptyExp exp1) = do
  x1 <- valueOf exp1
  return $ do
    v1 <- x1 >>= ExpVal.toList
    return $ ExpVal.Bool (null v1)
