module ExpVal where

data ExpVal
  = NumVal Int
  | BoolVal Bool
  deriving (Eq, Show)

expValToNum (NumVal n) = n

expValToBool (BoolVal b) = b
