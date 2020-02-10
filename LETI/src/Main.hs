import           Control.Monad.State
import           Data.Map
import           Env
import           Exp
import           ExpVal

main = do
  print $ evalState (valueOf prog1) initialEnv
  where
    initialEnv = fromList [("i", NumVal 1), ("v", NumVal 5), ("x", NumVal 10)]
    prog1 -- should evaluate to 3
     =
      DiffExp
        (DiffExp (VarExp "x") (ConstExp 3))
        (DiffExp (VarExp "v") (VarExp "i"))
