import           Env
import           Exp
import           ExpVal

initialEnv =
  extendList [("i", NumVal 1), ("v", NumVal 5), ("x", NumVal 10)] empty

main = do
  print $ valueOf prog1 initialEnv
  where
    prog1 -- should evaluate to 3
     =
      DiffExp
        (DiffExp (VarExp "x") (ConstExp 3))
        (DiffExp (VarExp "v") (VarExp "i"))
