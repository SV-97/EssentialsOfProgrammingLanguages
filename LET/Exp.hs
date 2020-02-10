module Exp where

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

valueOf :: Exp -> Env -> ExpVal
valueOf (ConstExp n) _ = NumVal n
valueOf (VarExp var) ρ = apply ρ var
valueOf (DiffExp exp1 exp2) ρ = NumVal $ v1 - v2
  where
    v1 = expValToNum $ valueOf exp1 ρ
    v2 = expValToNum $ valueOf exp2 ρ
valueOf (IsZeroExp exp) ρ = BoolVal (valueOf exp ρ == NumVal 0)
valueOf (IfExp exp1 exp2 exp3) ρ =
  if val1
    then valueOf exp2 ρ
    else valueOf exp3 ρ
  where
    BoolVal val1 = valueOf exp1 ρ
valueOf (LetExp var exp1 body) ρ1 = valueOf body ρ2
  where
    ρ2 = extend var (valueOf exp1 ρ1) ρ1
