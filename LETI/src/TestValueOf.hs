{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Monad.State
import           Data.Foldable       (foldl')
import           Data.Map
import           Env
import           Exp
import           ExpVal              (ExpVal)
import qualified ExpVal
import           System.Exit

type ErrorCode = ExitCode

instance Monoid ErrorCode where
  mempty = ExitSuccess
  err@(ExitFailure _) `mappend` _ = err
  ExitSuccess `mappend` x = x

type Test = IO ErrorCode

dbg :: (Show a) => String -> a -> IO a
dbg s a = do
  putStr s
  print a
  return a

dbg1 x = do
  dbg "Actual Out: " x
  putStrLn ""

errorText :: String -> String -> String -> IO ()
errorText name code expectedOut = do
  putStrLn $ "\nTest " ++ name ++ ":"
  putStrLn code
  putStrLn $ "\nExpected Out: " ++ expectedOut

makeTest :: String -> String -> Maybe ExpVal -> Env -> Exp -> Test
makeTest name code expectedOut initialEnv prog =
  let val = evalState (valueOf prog) initialEnv
   in if val == expectedOut
        then return ExitSuccess
        else do
          errorText name code (show expectedOut)
          dbg1 val
          return $ ExitFailure 1

testBase =
  makeTest
    "Base"
    "let i = 1, v = 5, x = 10 in\n(x - 3) - (v - i)"
    expectedOut
    env
    program
  where
    expectedOut = Just (ExpVal.Num 3)
    env =
      fromList [("i", ExpVal.Num 1), ("v", ExpVal.Num 5), ("x", ExpVal.Num 10)]
    program =
      DiffExp
        (DiffExp (VarExp "x") (ConstExp 3))
        (DiffExp (VarExp "v") (VarExp "i"))

testLet1 = makeTest "Let1" "let x = 10 in x" expectedOut env program
  where
    expectedOut = Just (ExpVal.Num 10)
    env = fromList []
    program = LetExp "x" (ConstExp 10) (VarExp "x")

testLet2 =
  makeTest
    "Let2"
    "let x = 15 in\n(let x = 10 in x - 2) - (x - 3)"
    expectedOut
    env
    program
  where
    expectedOut = Just (ExpVal.Num (-4))
    env = fromList [("x", ExpVal.Num 15)]
    program =
      DiffExp
        (LetExp "x" (ConstExp 10) (DiffExp (VarExp "x") (ConstExp 2)))
        (DiffExp (VarExp "x") (ConstExp 3))

testCons = makeTest "Cons" "3 : []" expectedOut env program
  where
    expectedOut = Just (ExpVal.List [ExpVal.Num 3])
    env = fromList [("x", ExpVal.Num 15)]
    program = ConsExp (ConstExp 3) EmptyListExp

testHead = makeTest "Head" "head $ 5 : 3 : []" expectedOut env program
  where
    expectedOut = Just (ExpVal.Num 5)
    env = fromList [("x", ExpVal.Num 15)]
    program = HeadExp $ ConsExp (ConstExp 5) (ConsExp (ConstExp 3) EmptyListExp)

testTail = makeTest "Tail" "tail $ 1 : 5 : 3 : []" expectedOut env program
  where
    expectedOut = Just (ExpVal.List [ExpVal.Num 5, ExpVal.Num 3])
    env = fromList [("x", ExpVal.Num 15)]
    program =
      TailExp $
      ConsExp
        (ConstExp 1)
        (ConsExp (ConstExp 5) (ConsExp (ConstExp 3) EmptyListExp))

testIsEmpty1 =
  makeTest "isEmpty1" "isEmpty $ 1 : 5 : 3 : []" expectedOut env program
  where
    expectedOut = Just (ExpVal.Bool False)
    env = fromList [("x", ExpVal.Num 15)]
    program =
      IsEmptyExp $
      ConsExp
        (ConstExp 1)
        (ConsExp (ConstExp 5) (ConsExp (ConstExp 3) EmptyListExp))

testIsEmpty2 = makeTest "isEmpty2" "isEmpty $ []" expectedOut env program
  where
    expectedOut = Just (ExpVal.Bool True)
    env = fromList [("x", ExpVal.Num 15)]
    program = IsEmptyExp EmptyListExp

testAlwaysFails :: Test
testAlwaysFails = return (ExitFailure 1)

main = do
  testResults <-
    sequence
      [ testBase
      , testLet1
      , testLet2
      , testCons
      , testHead
      , testTail
      , testIsEmpty1
      , testIsEmpty2
      ]
  exitWith $ mconcat testResults
