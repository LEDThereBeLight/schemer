module Seasoned where

import Control.Conditional(cond)

main = print "Ready for some lisp action"

isMember _ [] = False
isMember x (y:ys)
  | x == y    = True
  | otherwise = isMember x ys

-- Let's make a function that tells us if a list has two elements in a row

twoInARow [] = False
twoInARow (x:xs)
  | isFirst x xs = True
  | otherwise    = twoInARow xs

isFirst x []     = False
isFirst x (y:ys) = x == y

-- twoInARow is great, but it continues searching the rest of the list even if isFirst already found out that the list is empty. Maybe it makes sense to leave the decision of whether or not to continue searching to isFirst.

twoInARow' [] = False
twoInARow' (x:xs) = isFirst' x xs

isFirst' x [] = False
isFirst' x (y:ys)
  | x == y    = True
  | otherwise = twoInARow' (y:ys)

-- The thing is, whenever isFirst' calls twoInARow' in the otherwise statement, it already knows the list is not empty so it just goes straight back into isFirst'. That means we can recur directly into isFirst' instead. This new twoInARow function will need to keep track of the value preceding the current list, so we'll need a helper.

twoInARow'' []     = False
twoInARow'' (x:xs) = helper x xs
  where helper _         []     = False
        helper preceding (y:ys)
          | preceding == y      = True
          | otherwise           = helper y ys

-- Let's move on to sumOfPrefixes. In order to get the sum of all previous elements and the next number, we need a helper that keeps track of the sum of numbers seen so far. We'll write the helper first.

sumOfPrefixesHelper _      []     = []
sumOfPrefixesHelper sonssf (x:xs) = (sonssf + x) : sumOfPrefixesHelper (sonssf + x) xs

-- The final sumOfPrefixes function, then, is...

sumOfPrefixes xs = helper 0 xs
  where helper _      []     = []
        helper sonssf (x:xs) = (sonssf + x) : helper (sonssf + x) xs

{- Now we've got a "scramble" function.
scramble [1,1,1,3,4,2,1,1,9,2] = [1,1,1,1,1,4,1,1,1,9]
What the hell is this?
Okay, I guess it treats each list element as an index from its own position and returns the list element at the position starting from the current position and counting backwards.
But... why? -}

-- What's the prefix of [4,2,1,1,9,2] if the list is [1,1,1,3,4,2,1,1,9,2]?
-- [1,1,1,3,4], since the prefix includes the first element

-- scramble needs to use the prefix function because we need to start counting backwards in the prefix of the current element.

-- How is scramble similar to sumOfPrefixes, then?
-- scramble needs information about the list seen so far, just like sumOfPrefixes. The difference is it needs the full prefix of elements seen so far, whereas sumOfPrefixes just needs the sum of the elements seen so far.

-- Okay now we're talking about a "pick" function, so I guess we're gonna need that too.
-- What is (pick n lat) where n is 4 and lat is [4,3,1,1,1]? 1
-- What is (pick 2 [2,4,3,1,1,1])? 4

pick :: (Eq a, Num a) => a -> [a] -> a
pick _ []     = error "pick of empty list"
pick n (x:xs)
  | n == 1    = x
  | otherwise = pick (n-1) xs

-- Let's write the scrambleHelper subroutine, and then use it for the higher level scramble function.

scrambleHelper []          _      = []
scrambleHelper list@(x:xs) revPre = (pick x (x:revPre))
                                  : scrambleHelper xs (x:revPre)


-- Now we can finally write scramble!
scramble :: (Eq a, Num a) => [a] -> [a]
scramble list = helper list []
  where helper []     _      = []
        helper (n:ns) revPre = pick n (n:revPre)
                             : helper ns (n:revPre)

-- Okay, now we're back to using the Y Combinator. Why do I put myself through this?

-- So we have this new "letrec" construct for some reason. There's clearly something different between using letrec and defining recursive helper functions, but I'm not seeing the big picture here. Maybe you can't access the outer variable scope in lisp helper functions? idk.

-- Alright, after googling it a bit even the stackoverflow answers aren't making a lot of sense to me. It seems like using letrec allows you to make recursive calls without having to provide all of the arguments explicitly, which people are convinced makes the function more readable. I still don't understand the benefit though, I must be missing something. Whatevs, I'll just keep going and maybe it'll click later.

-- I found an article explaining how lisp's letrec is actually haskell's let. Haskell doesn't have a lispy let. I'm not clear on the differences though: http://neilmitchell.blogspot.com/2007/03/let-vs-letrec.html.


-- 13. Hop, skip and jump.

-- Okay we're intersecting stuff now. Here's our old definition:

intersect [] _ = []
intersect _ [] = []
intersect (x:xs) set2
  | isMember x set2 = x : intersect xs set2
  | otherwise       =     intersect xs set2

-- Now that we've learned letrec (let in haskell) we've got a new way to write this, I guess.

intersect' set1 set2 =
  let i = (\set ->
            cond [ (null set, [])
                 , (isMember (head set) set2, (head set) : i (tail set))
                 , (otherwise, i (tail set))
                 ])
  in i set1

-- Much better! 🤔
