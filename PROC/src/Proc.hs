module Proc where

import           Data.Map (Map)
import qualified Data.Map as Map

type Var = String

data Value
  = Num Int
  | Bool Bool
  | List [Value]
  | Procedure Env Var Expr
  deriving (Eq, Show)

toInt :: Value -> Maybe Int
toInt (Num n) = Just n
toInt _       = Nothing

toBool :: Value -> Maybe Bool
toBool (Bool b) = Just b
toBool _        = Nothing

toList :: Value -> Maybe [Value]
toList (List l) = Just l
toList _        = Nothing

toProcComponents :: Value -> Maybe (Env, Var, Expr)
toProcComponents (Procedure env var expr) = Just (env, var, expr)
toProcComponents _                        = Nothing

type Env = Map Var Value

empty :: Env
empty = Map.empty

extend :: Env -> Var -> Value -> Env
extend r1 key value = Map.insert key value r1

extendList :: Env -> [(Var, Value)] -> Env
extendList r1 lst = Map.union (Map.fromList lst) r1

apply :: Env -> Var -> Maybe Value
apply = flip Map.lookup

data Expr
  = Const Int
  | Diff Expr Expr
  | Var Var
  | IsZero Expr
  | Let Var Expr Expr
  | EmptyList
  | Cons Expr Expr
  | Head Expr
  | Tail Expr
  | IsEmpty Expr
  | Lambda Var Expr
  | Application Expr Expr
  deriving (Eq, Show)

eval r e = snd $ valueOf r e

infixr 0 $.

($.) :: a -> b -> (a, b) -- essentially shorthand for `(,) a $ b`
a $. b = (a, b)

valueOf :: Env -> Expr -> (Env, Maybe Value)
valueOf r1 (Const n) = (r1, Just $ Num n)
valueOf r1 (Diff expr1 expr2) =
  r1 $. do
    val1 <- eval r1 expr1 >>= toInt
    val2 <- eval r1 expr2 >>= toInt
    Just . Num $ val1 + val2
valueOf r1 (Var var) = (r1, apply r1 var)
valueOf r1 (IsZero expr1) =
  r1 $.
  let val1 = eval r1 expr1
   in Bool <$> ((==) <$> val1 <*> pure (Num 0))
valueOf r1 (Let var expr1 body) =
  r1 $. do
    val1 <- eval r1 expr1
    let r2 = extend r1 var val1
    eval r2 body
valueOf r1 EmptyList = (r1, Just $ List [])
valueOf r1 (Cons expr1 expr2) =
  r1 $. do
    val1 <- eval r1 expr1
    val2 <- eval r1 expr2 >>= toList
    return $ List (val1 : val2)
valueOf r1 (Head expr1) =
  r1 $. do
    val1 <- eval r1 expr1 >>= toList
    return $ head val1
valueOf r1 (Tail expr1) =
  r1 $. do
    val1 <- eval r1 expr1 >>= toList
    return $ List $ tail val1
valueOf r1 (IsEmpty expr1) =
  r1 $. do
    val1 <- eval r1 expr1 >>= toList
    return $ Bool $ null val1
valueOf r1 (Lambda var expr) = r1 $. Just $ Procedure r1 var expr
valueOf r1 (Application expr1 expr2) =
  (,) r1 $ do
    (fEnv, fVar, fBody) <- eval r1 expr1 >>= toProcComponents
    val2 <- eval r1 expr2
    let r2 = extend fEnv fVar val2
    eval r2 fBody
