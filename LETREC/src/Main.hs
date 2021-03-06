module Main where

import           Data.Map as Map
import           LetRec

main :: IO ()
main = print $ eval Map.empty program

{-
let add = \a -> \b -> a - (0 - b)
in let sum l = if isEmpty l then 0 else add (head l) (sum (tail l))
  in sum (1:2:3[])
-}
program =
  Let
    "add"
    addFunc
    (LetRec
       "sum"
       sumFunc
       (Application
          (Var "sum")
          (Cons (Const 1) (Cons (Const 2) (Cons (Const 3) EmptyList)))))
  where
    addFunc =
      Lambda "a" (Lambda "b" (Diff (Var "a") (Diff (Const 0) (Var "b"))))
    sumFunc =
      Lambda
        "l"
        (If
           (IsEmpty (Var "l"))
           (Const 0)
           (Application
              (Application (Var "add") (Head (Var "l")))
              (Application (Var "sum") (Tail (Var "l")))))
