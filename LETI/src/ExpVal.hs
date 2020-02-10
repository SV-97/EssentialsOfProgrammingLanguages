module ExpVal where

data ExpVal
  = NumVal Int
  | BoolVal Bool
  deriving (Eq, Show)

expValToNum :: ExpVal -> Maybe Int
expValToNum (NumVal n) = Just n
expValToNum _          = Nothing

expValToBool :: ExpVal -> Maybe Bool
expValToBool (BoolVal b) = Just b
expValToBool _           = Nothing
