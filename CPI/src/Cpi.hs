module Cpi where

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe  (fromJust)
import           Debug.Trace

type Var = String

type Env = Map Var Value

type FinalAnswer = Value

type Continuation = Value -> FinalAnswer

applyCont :: Continuation -> Value -> FinalAnswer
applyCont k = k

endCont :: Continuation
endCont = trace "End of computation" id

data Value
  = Num Int
  | Bool Bool
  | Procedure Env Var Expr
  deriving (Eq, Show)

toInt :: Value -> Int
toInt (Num n) = fromJust $ Just n
toInt _       = fromJust Nothing

toBool :: Value -> Bool
toBool (Bool b) = fromJust $ Just b
toBool _        = fromJust Nothing

toProcComponents :: Value -> (Env, Var, Expr)
toProcComponents (Procedure env var expr) = fromJust $ Just (env, var, expr)
toProcComponents _                        = fromJust Nothing

empty :: Env
empty = Map.empty

extend :: Env -> Var -> Value -> Env
extend r1 key value = Map.insert key value r1

extendList :: Env -> [(Var, Value)] -> Env
extendList r1 lst = Map.union (Map.fromList lst) r1

apply :: Env -> Var -> Value
apply r var = fromJust $ Map.lookup var r

data Expr
  = Const Int
  | Diff Expr Expr
  | Var Var
  | IsZero Expr
  | IfExp Expr Expr Expr
  | Let Var Expr Expr
  | Lambda Var Expr
  | Application Expr Expr
  deriving (Eq, Show)

valueOfProgram :: Expr -> FinalAnswer
valueOfProgram expr = valueOf r expr k
  where
    r = Map.empty
    k = endCont

valueOf :: Env -> Expr -> Continuation -> Value
valueOf r1 (Const n) k = applyCont k $ Num n
valueOf r1 (Var var) k = applyCont k $ apply r1 var
valueOf r1 (Lambda var expr) k = applyCont k $ Procedure r1 var expr
valueOf r1 (IsZero expr1) k = valueOf r1 expr1 $ zeroCont k
  where
    zeroCont k val = applyCont k $ Bool $ val == Num 0
valueOf r1 (Let var expr1 body) k = valueOf r1 expr1 $ letCont var body r1 k
  where
    letCont var body r1 k val = valueOf (extend r1 var val) body k
valueOf r1 (IfExp expr1 expr2 expr3) k =
  valueOf r1 expr1 $ ifCont expr2 expr3 r1 k
  where
    ifCont expr2 expr3 r1 k val =
      if toBool val
        then valueOf r1 expr2 k
        else valueOf r1 expr3 k
valueOf r1 (Diff expr1 expr2) k = valueOf r1 expr1 $ diffCont1 expr2 r1 k
  where
    diffCont1 expr2 r1 k val1 = valueOf r1 expr2 $ diffCont2 expr2 val1 k
    diffCont2 expr2 val1 k val2 = applyCont k $ Num $ toInt val1 - toInt val2
valueOf r1 (Application f x) k = valueOf r1 f $ operandCont r1 x k
  where
    operandCont r1 x k valFunc = valueOf r1 x $ operatorCont valFunc k
    operatorCont valFunc k valArg =
      let (fEnv, fVar, fBody) = toProcComponents valFunc
       in valueOf (extend fEnv fVar valArg) fBody k
