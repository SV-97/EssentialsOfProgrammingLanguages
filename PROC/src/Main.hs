module Main where

import           Data.Map as Map
import           Proc

main :: IO ()
main = print $ eval Map.empty program

{-
let add = \a -> \b -> a - (0 - b) in add 5 10
-}
program =
  Let
    "add"
    (Lambda "a" (Lambda "b" (Diff (Var "a") (Diff (Const 0) (Var "b")))))
    (Application (Application (Var "add") (Const 5)) (Const 10))
