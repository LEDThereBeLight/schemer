module TheLittleSchemer where

import Data.List

main = "ready for some scheme"

-- Atoms are little things

-- Lists are groups of atoms or s-expressions

-- s-expressions are groupings of atoms and lists

-- car, cdr, cons

-- atom?, eq?, lat?
-- lat is a list of atoms

-- lat? is not needed, because Haskell is statically typed

-- null? checks for an empty list, and it's always the first question we ask when recursing on a list

-- else and or are functions, and so is member?

-- rember removes a member. Let's write it in a lispy way
rember :: Eq a => a -> [a] -> [a]
rember =
  \a lat ->
    case () of
    ()  | null lat -> []
        | a == head lat -> tail lat
        | otherwise -> head lat : rember a (tail lat)

-- And now in a more haskelly way

rember' a [] = []
rember' a (x:xs)
  | a == x    = xs
  | otherwise = x : rember a xs

-- firsts takes a list of lists and returns the head of each as a list

firsts [] = []
firsts (l:ls)
  | null l    = firsts ls
  | otherwise = head l : firsts ls

-- alternatively, if we don't need to worry about empty list elements:
firsts' = map head

-- "insertR new old lat" inserts a new to the right of old in lat

insertR new old [] = []
insertR new old (x:xs)
  | old == x  = x : new : xs
  | otherwise = old : insertR new old xs

insertL new old [] = []
insertL new old (x:xs)
  | old == x  = new : x : xs
  | otherwise = old : insertL new old xs

-- (subst new old lat) replaces the first occurrence of old with new in lat

subst new old [] = []
subst new old (x:xs)
  | old == x  = new : xs
  | otherwise =   x : subst new old xs

-- subst' replaces either the first occurrence of old' or old'' with new in lat

subst' new old' old'' [] = []
subst' new old' old'' (x:xs)
  | old'  == x = new : xs
  | old'' == x = new : xs
  | otherwise  =   x : subst' new old' old'' xs

-- (multirember a lat) removes hella a's from lat
multirember a [] = []
multirember a (x:xs)
  | a == x    =     multirember a xs
  | otherwise = x : multirember a xs

multiinsertR new old [] = []
multiinsertR new old (x:xs)
  | old == x  = x : new : multiinsertR new old xs
  | otherwise = x       : multiinsertR new old xs

multiinsertL new old [] = []
multiinsertL new old (x:xs)
  | old == x  = new : x : multiinsertL new old xs
  | otherwise =       x : multiinsertL new old xs

multisubst new old [] = []
multisubst new old (x:xs)
  | old == x  = new : multisubst new old xs
  | otherwise =   x : multisubst new old xs

-- now we're defining add1, sub1, and (+) but we don't really have the same primitives in haskell

-- addtup sums all the numbers in a list
addtup [] = 0
addtup (n:ns) = n + addtup ns

addtup' :: (Foldable t, Num a) => t a -> a
addtup' = sum

-- tupPlus takes two lists of numbers and creates a new list by adding together the heads of each of the two inputs

tupPlus [] _ = []
tupPlus _ [] = []
tupPlus (n:ns) (m:ms) = (n+m) : tupPlus ns ms

tupPlus' first second = zipWith (+) first second

-- Define the greater than operator for positive numbers
(>!) :: (Eq a, Num a) => a -> a -> Bool
(>!) 0 m = False
(>!) n 0 = True
(>!) n m = (>!) (n-1) (m-1)

-- How about ≤

(≥) :: (Eq a, Num a) => a -> a -> Bool
n ≥ 0 = True
0 ≥ m = m == 0
n ≥ m = (n-1) ≥ (m-1)

length' [] = 0
length' (x:xs) = 1 + length' xs

-- pick gets the element at a location

pick _ [] = error "pick of empty list"
pick 1 (x:xs) = x
pick n (x:xs) = pick (n-1) xs

-- rempick removes the element at a location

rempick _ [] = error "rempick of empty list"
rempick 1 (x:xs) = xs
rempick n (x:xs) = x : rempick (n-1) xs

-- occur counts the occurrences of a in lat

occur _ [] = 0
occur a (x:xs)
  | a == x    = 1 + occur a xs
  | otherwise =     occur a xs

occur' a = foldl' (\acc x -> if a == x then acc + 1 else acc) 0
