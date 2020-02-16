-- something is wrong with this but no idea what it is
module Lecad where

import           Control.Applicative (liftA2)
import           Control.Monad.State
import           Data.List           (elemIndex)

type Var = String

data Value
  = Num Int
  | Bool Bool
  | List [Value]
  | Procedure RuntimeEnv NamelessExpr
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

toProcComponents :: Value -> Maybe (RuntimeEnv, NamelessExpr)
toProcComponents (Procedure env expr) = Just (env, expr)
toProcComponents _                    = Nothing

type StaticEnv = [Var]

empty :: StaticEnv
empty = []

extend :: StaticEnv -> Var -> StaticEnv
extend = flip (:)

apply :: StaticEnv -> Var -> Maybe Int
apply = flip elemIndex

data Expr
  = Const Int
  | Diff Expr Expr
  | Var Var
  | IsZero Expr
  | Let Var Expr Expr
  | Lambda Var Expr
  | Application Expr Expr
  deriving (Eq, Show)

type LexicalAdress = Int

data NamelessExpr
  = NConst Int
  | NDiff NamelessExpr NamelessExpr
  | NVar LexicalAdress
  | NIsZero NamelessExpr
  | NLet NamelessExpr NamelessExpr
  | NLambda NamelessExpr
  | NApplication NamelessExpr NamelessExpr
  deriving (Eq, Show)

toNameless :: Expr -> Maybe NamelessExpr
toNameless = toNamelessExpr empty

toNamelessExpr :: StaticEnv -> Expr -> Maybe NamelessExpr -- maybe record highest index inside a State and use that to create a fixed size array for the runtime
toNamelessExpr env (Const n) = Just $ NConst n
toNamelessExpr env (Diff e1 e2) =
  liftA2 NDiff (toNamelessExpr env e1) (toNamelessExpr env e2)
toNamelessExpr env (Var var) = NVar <$> apply env var
toNamelessExpr env (IsZero e) = NIsZero <$> toNamelessExpr env e
toNamelessExpr env (Let var e body) =
  liftA2 NLet (toNamelessExpr env e) (toNamelessExpr (extend env var) e)
toNamelessExpr env (Lambda var body) =
  NLambda <$> toNamelessExpr (extend env var) body
toNamelessExpr env (Application f x) =
  liftA2 NApplication (toNamelessExpr env f) (toNamelessExpr env x)

type RuntimeEnv = [Value]

emptyREnv :: RuntimeEnv
emptyREnv = []

extendREnv :: Value -> State RuntimeEnv ()
extendREnv x = do
  old <- get
  put (x : old)

applyREnv :: Int -> State RuntimeEnv Value
applyREnv i = do
  env <- get
  return $ env !! i

valueOf :: NamelessExpr -> State RuntimeEnv (Maybe Value)
valueOf (NConst n) = return $ Just $ Num n
valueOf (NDiff expr1 expr2) = do
  val1 <- valueOf expr1
  val2 <- valueOf expr2
  let v1 = val1 >>= toInt
  let v2 = val2 >>= toInt
  return $ Num <$> liftA2 (-) v1 v2
valueOf (NVar adr) = Just <$> applyREnv adr
valueOf (NIsZero expr) = do
  val1 <- valueOf expr
  return $ Bool <$> liftA2 (==) val1 (pure $ Num 0)
valueOf (NLet expr1 body) = do
  val1 <- valueOf expr1
  old <- get
  case val1 of
    Just v -> do
      result <- extendREnv v >> valueOf body
      put old
      return result
    Nothing -> return Nothing
valueOf (NLambda body) = do
  env <- get
  return $ Just $ Procedure env body
valueOf (NApplication f x) = do
  fVal <- valueOf f
  arg <- valueOf x
  case (fVal >>= toProcComponents, arg) of
    (Just (fEnv, fBody), Just arg) -> do
      env <- get
      extendREnv arg
      result <- valueOf fBody
      put env
      return result
    _ -> return Nothing
