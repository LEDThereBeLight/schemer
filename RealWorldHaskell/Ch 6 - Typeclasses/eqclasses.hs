module Eqclasses where

import Data.Char

data Color = Red
           | Green
           | Blue

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance Eq Color where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _ == _ = False

-- Read is more difficult
instance Read Color where
  readsPrec _ value =
    tryParse [ ("Red", Red)
             , ("Green", Green)
             , ("Blue", Blue)
             ]
      where tryParse [] = []
            tryParse ((attempt, result):xs) =
              if (take (length attempt) value) == attempt
                then [(result, drop (length attempt) value)]
                else tryParse xs

main = do
  putStrLn "you down with opp?"
  inpStr <- getLine
  return ((toUpper . head $ inpStr) == 'Y')
