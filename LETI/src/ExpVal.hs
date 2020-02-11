module ExpVal where

data ExpVal
  = Num Int
  | Bool Bool
  | List [ExpVal]
  deriving (Eq, Show)

toNum :: ExpVal -> Maybe Int
toNum (Num n) = Just n
toNum _       = Nothing

toBool :: ExpVal -> Maybe Bool
toBool (Bool b) = Just b
toBool _        = Nothing

toList :: ExpVal -> Maybe [ExpVal]
toList (List l) = Just l
toList _        = Nothing
