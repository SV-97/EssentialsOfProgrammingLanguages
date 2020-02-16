module Main where

import           Cpi
import           Data.Map as Map

main :: IO ()
main = print $ valueOfProgram program

{-
let add = \a -> \b -> a - (0 - b) in add 5 10
-}
program =
  Let
    "add"
    (Lambda "a" (Lambda "b" (Diff (Var "a") (Diff (Const 0) (Var "b")))))
    (Application (Application (Var "add") (Const 5)) (Const 10))
