module Env
  ( Env
  , extend
  , apply
  , empty
  , extendList
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           ExpVal
import           LET      (Var)

type Env = Map Var ExpVal

empty :: Env
empty = Map.empty

extend :: Var -> ExpVal -> Env -> Env
extend = Map.insert

extendList :: [(Var, ExpVal)] -> Env -> Env
extendList lst old = Map.union new old
  where
    new = Map.fromList lst

apply :: Env -> Var -> ExpVal
apply = (Map.!)
