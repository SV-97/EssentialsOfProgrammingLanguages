module Exp where

import           Control.Monad.State
import           Env
import           ExpVal
import           LET

data Exp
  = ConstExp Int
  | DiffExp Exp Exp
  | VarExp Var
  | IsZeroExp Exp
  | IfExp Exp Exp Exp
  | LetExp Var Exp Exp

valueOf :: Exp -> State Env (Maybe ExpVal)
valueOf (ConstExp n) = return . Just $ NumVal n
valueOf (VarExp var) = apply var
valueOf (DiffExp exp1 exp2) = do
  x1 <- valueOf exp1
  x2 <- valueOf exp2
  let v1 = x1 >>= expValToNum
  let v2 = x2 >>= expValToNum
  return $ ((-) <$> v1 <*> v2) >>= Just . NumVal
valueOf (IsZeroExp exp) = do
  x1 <- valueOf exp
  return $ BoolVal <$> ((==) <$> x1 <*> pure (NumVal 0))
valueOf (IfExp exp1 exp2 exp3) = do
  val1 <- valueOf exp1
  case val1 of
    Just (BoolVal True)  -> valueOf exp2
    Just (BoolVal False) -> valueOf exp3
    _                    -> return Nothing
valueOf (LetExp var exp1 body) = do
  ρ1 <- get
  val1 <- valueOf exp1
  case val1 of
    Just v -> do
      extend var v
      let val = valueOf body
      put ρ1
      val
    Nothing -> return Nothing
