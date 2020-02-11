module Env
  ( Env
  , extend
  , apply
  , empty
  , extendList
  ) where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           ExpVal
import           LET                 (Var)

type Env = Map Var ExpVal

empty :: Env
empty = Map.empty

extend :: Var -> ExpVal -> State Env ()
extend key value = do
  ρ1 <- get
  put $ Map.insert key value ρ1

extendList :: [(Var, ExpVal)] -> State Env ()
extendList lst = do
  old <- get
  put $ Map.union (Map.fromList lst) old

apply :: Var -> State Env (Maybe ExpVal)
apply var = do
  ρ <- get
  return $ var `Map.lookup` ρ
