module Main (main) where

import SimpleJSON
import PutJSON

object = JObject [ ("foo", JNumber 1)
                 , ("bar", JBool False)
                 ]

main = putJValue object
