module Main where

import Data.Char
import Data.List

expandedForm :: Int -> String
expandedForm n = unwords . intersperse " + " . map show $ numbers
  where numbers = reverse noZeroes
        noZeroes = filter (/= 0) expandedNumbers
        expandedNumbers = map (\tup -> (fst tup) * 10 ^ (snd tup)) pairsWithPowers
        pairsWithPowers = zip (map digitToInt (reverse . show $ n)) [0..]

makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x:xs) | x `elem` xs = makeSet xs
               | otherwise   = x : makeSet xs

subSet :: Eq a => [a] -> [a] -> Bool
subSet []  _ = True
subSet (x:xs) ys = x `elem` ys && subSet xs ys

eqSet xs ys = subSet xs ys && subSet ys xs

intersection [] _ = []
intersection _ [] = []
intersection (x:xs) ys | x `elem` ys = x : intersection xs ys
                       | otherwise   = intersection xs ys

intersectAll :: Eq a => [[a]] -> [a]
intersectAll = foldr1 intersection

union' [] ys = ys
union' xs [] = xs
union' (x:xs) ys | x `elem` ys = union' xs ys
                 | otherwise   = x : union' xs ys

multiremberAndCo :: Eq a => a -> [a] -> ([a] -> [a] -> x) -> x
multiremberAndCo a lat col
  | null lat      = col
                    []
                    []
  | head lat == a = multiremberAndCo a
                                     (tail lat)
                                     (\newlat seen -> col newlat
                                                          (head lat : seen))
  | otherwise     = multiremberAndCo a
                                     (tail lat)
                                     (\newlat seen -> col (head lat : newlat)
                                                          seen)

aFriend :: Eq a => [a] -> [a] -> Bool
aFriend x y = null y

lastFriend :: Eq a => [a] -> [a] -> Int
lastFriend x y = length x


looking a lat = keepLooking a (pick 1 lat) lat

keepLooking a sorn lat | isNumber sorn = keepLooking a (pick sorn lat) lat
                       | otherwise     = a == sorn

eternity x = eternity x

-- length l | null l = 0
--          | otherwise = 1 + length (tail list)
--
-- \l -> if null l then 0
--       else 1 + ... (tail l)
--
-- \l -> if null l then 0
--       else 1 + (\l -> if null l then 0
--                       else 1 + ... (tail l))
--                (tail l)
--
-- \length -> \l -> if null l then 0
--                  else 1 + length (tail l)
-- ???
--
--
--
-- \f -> \l -> if null l then 0
--             else 1 + f (tail l)
-- \g -> \l -> if null l then 0
--             else 1 + g (tail l)
-- ???
--
--
--
--
-- \f -> \l -> if null l then 0
--             else 1 + f (tail l)
-- \g -> \l -> if null l then 0
--             else 1 + g (tail l)
-- \h -> \l -> if null l then 0
--             else 1 + h (tail l)
-- ???
--
--
-- \length -> \l -> if null l then 0
--                  else 1 + length (tail l)
-- \length -> \l -> if null l then 0
--                  else 1 + length (tail l)
-- \length -> \l -> if null l then 0
--                  else 1 + length (tail l)
-- ???
--
--
--
twoInARow [] = False
twoInARow (x:xs) = isFirstB x xs

isFirstB _ [] = False
isFirstB x (y:ys) | x == y = True
                  | otherwise = twoInARow (y:ys)

isFirst _ [] = False
isFirst x (y:ys) = x == y



main = putStrLn "Hey"
