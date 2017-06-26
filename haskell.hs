module Main where

main = getLine
   >>= \l1 -> getLine
   >>= \l2 -> putStrLn (l1++l2)
