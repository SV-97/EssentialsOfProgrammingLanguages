module Main where

import           Control.Monad.State
import           Lecad

main :: IO ()
main =
  case toNameless program
    -- Just success -> print success -- evalState (valueOf success) []
        of
    Just success -> print $ evalState (valueOf prog) []
    Nothing      -> putStrLn "Nothing"

{-
let add = \a -> \b -> a - (0 - b) in add 5 10
-}
program =
  Let
    "add"
    (Lambda "a" (Lambda "b" (Diff (Var "a") (Diff (Const 0) (Var "b")))))
    (Application (Application (Var "add") (Const 5)) (Const 10))

prog =
  NLet
    (NLambda (NLambda (NDiff (NVar 0) (NDiff (NConst 0) (NVar 1)))))
    (NApplication (NApplication (NVar 0) (NConst 5)) (NConst 10))
