module Dict (
  fromList,
  toList,
  Dict.foldl,
  size,
  isEmpty,
  get,
  delete,
  insert
) where

-- for "fromList"
import Data.List (foldl)
import Data.Maybe

{-|
  In creating a dictionary, we have two real options: a hash table, and a tree. Each choice has pros and cons, and the choice depends on the application.

  Let's assume we're looking for a general purpose use-case where data doesn't need to be encrypted (if we need encryption or security, hashing is probably the way to go).

  Speed comparison:

            Hash Table    Rebalancing Tree
  Search       O(1)           O(lg n)
  Insert       O(1)           O(lg n)
  Delete       O(1)           O(lg n)
  Next         O(n)           O(lg n)
  Previous     O(n)           O(lg n)

  I see two main benefits in favor of rebalancing trees over hash tables. The first is that the keys are ordered, so it's much faster to work with sets where you might need to get the "next" or "previous" keys.

  The second benefit is that trees use pointers, so I'm going to say that the speed is more of a "guarantee" than with a hash table. In a web-based service (like Medallia) that involves heavy user interaction, we'll never run into slowdowns when a new array needs to be allocated or moved.

  The downside of using a tree is that we're trading O(1) speed for O(lg n), but that seems like a fair trade given modern computer speeds. Our keys also need to be comparable, otherwise we won't know where to put them.

  Given these benefits, in our "general purpose" use case, I suggest using a red-black rebalancing tree because of its ease of implementation and guaranteed O(lg n) time.

  Red Black trees satisfy two invariants:
    1. No red node has a red parent
    2. Every path from the root to an BlackLeaf node has the same number of black nodes

  Together, these invariants ensure that the length of the longest path from the root to a leaf is no longer than twice the length of the shortest path from the root to a leaf. Sparing the math, this guarantees O(lg n) time for common operations.

  BlackLeaf nodes are considered to be Black, and contribute to the height of the tree.

  We'll use the Okasaki's red-black tree from https://wiki.rice.edu/confluence/download/attachments/2761212/Okasaki-Red-Black.pdf
-}

main = print $ get 1 $ fromList [(1, "hey"), (2, "blow"), (6, "there"), (4, "sup")]

-- private stuff

data Color = Red
           | Black
           | DoubleBlack
           | NegativeBlack
  deriving (Eq, Show)

data Tree k v = BlackLeaf
              | DoubleBlackLeaf
              | Tree Color (Tree k v) k v (Tree k v)
  deriving (Eq, Show)

type Dict k v = Tree k v

blacker :: Color -> Color
blacker NegativeBlack = Red
blacker Red = Black
blacker Black = DoubleBlack
blacker DoubleBlack = error "too much black"

blacker' :: Tree k v -> Tree k v
blacker' BlackLeaf = DoubleBlackLeaf
blacker' (Tree color l k v r) = Tree (blacker color) l k v r

redder :: Color -> Color
redder NegativeBlack = error "too much red"
redder Red = NegativeBlack
redder Black = Red
redder DoubleBlack = Black

redder' :: Tree k v -> Tree k v
redder' DoubleBlackLeaf = BlackLeaf
redder' (Tree color l k v r) = Tree (redder color) l k v r

blacken :: Tree k v -> Tree k v
blacken BlackLeaf = BlackLeaf
blacken DoubleBlackLeaf = DoubleBlackLeaf
blacken (Tree _ a k v b) = Tree Black a k v b

redden :: Tree k v -> Tree k v
redden BlackLeaf = error "can't redden empty tree"
redden DoubleBlackLeaf = error "can't redden empty tree"
redden (Tree _ a k v b) = Tree Red a k v b

isDoubleBlack :: Tree k v -> Bool
isDoubleBlack DoubleBlackLeaf = True
isDoubleBlack (Tree DoubleBlack _ _ _ _) = True
isDoubleBlack _ = False

bubble :: Color -> Tree k v -> k -> v -> Tree k v -> Tree k v
bubble color l k v r
  | isDoubleBlack l || isDoubleBlack r = balance (blacker color) (redder' l) k v (redder' r)
  | otherwise                          = balance color l k v r


{-| Public stuff... mostly. some helpers included -}


{-| Insertion
When inserting a new node into an empty position, we color it red (to maintain invariant 2) and make sure the root is black.

Inserting red nodes may cause a violation of invariant 1 above, so it will go to the black grandparent of the newly inserted offending node to fix the violation.

There are four cases where two red nodes can be in a row. Fortunately, all of them can be resolved in the same way.

(Capital letters are black, lowercase are red)

      1.        2.    3.       4.
      Z         Z     X         X
    x         y         y         z
      y     x             z     y

The "solution" is to balance them like this:

      y
    X   Z
    (yay!)

This solution puts a red node at the top, which might again cause two reds in a row, so we continue balancing up the tree to the root, which we force to black.
-}

insert :: Ord k => k -> v -> Dict k v -> Dict k v
insert key value dict = makeBlack (insertHelper dict)
  where insertHelper BlackLeaf = Tree Red BlackLeaf key value BlackLeaf
        insertHelper (Tree color left k v right)
          | key <  k = balance color (insertHelper left) k v right
          | key == k = Tree color left key value right -- overwrites existing
          | key >  k = balance color left k v (insertHelper right)
        makeBlack (Tree _ left k v right) = Tree Black left k v right

delete :: (Ord k, Show k) => k -> Tree k v -> Tree k v
delete k s = blacken (del s)
  where del BlackLeaf = BlackLeaf
        del s@(Tree color l yk yv r) | k < yk = bubble color (del l) yk yv r
                                     | k > yk = bubble color l yk yv (del r)
                                     | otherwise = remove s

remove :: Tree k v -> Tree k v
remove BlackLeaf = BlackLeaf
remove (Tree Red BlackLeaf _ _ BlackLeaf) = BlackLeaf
remove (Tree Black BlackLeaf _ _ BlackLeaf) = DoubleBlackLeaf
remove (Tree Black BlackLeaf _ _ (Tree Red l xk xv r)) = Tree Black l xk xv r
remove (Tree Black (Tree Red l xk xv r) _ _ BlackLeaf) = Tree Black l xk xv r
remove (Tree color l yk yv r) = bubble color l' maxXk maxXv r
  where (maxXk, maxXv) = Dict.max l
        l' = removeMax l

removeMax :: Tree k v -> Tree k v
removeMax BlackLeaf = error "no max in empty tree"
removeMax s@(Tree _ _ _ _ BlackLeaf) = remove s
removeMax s@(Tree color l xk xv r) = bubble color l xk xv (removeMax r)

-- For reference, the pattern is Tree color left key value right
-- 1
balance :: Color -> Tree k v -> k -> v -> Tree k v -> Tree k v
balance Black (Tree Red a xk xv (Tree Red b yk yv c)) zk zv d =
  Tree Red (Tree Black a xk xv b) yk yv (Tree Black c zk zv d)
-- 2
balance Black (Tree Red (Tree Red a xk xv b) yk yv c) zk zv d =
  Tree Red (Tree Black a xk xv b) yk yv (Tree Black c zk zv d)
-- 3
balance Black a xk xv (Tree Red b yk yv (Tree Red c zk zv d)) =
  Tree Red (Tree Black a xk xv b) yk yv (Tree Black c zk zv d)
-- 4
balance Black a xk xv (Tree Red (Tree Red b yk yv c) zk zv d) =
  Tree Red (Tree Black a xk xv b) yk yv (Tree Black c zk zv d)

{-| We also need six additional cases for balancing after a deletion. There are three total variants, each of which is mirrored.
(Capital letters are black, lower case are red)

    1.      2.      3.      4.            5.        6.
    Z (db)    Z (db)  X (db)  X (db)         Z (db)    X (db)
  x         y           z       y          x (nb)        z (nb)
    y    x           y           z      w    y        y   w


    Solution 1 (for cases 1-4)          Solution 2 (cases 5-6)
        Y                                     Y
      X  Z                                  X   Z
                                          w
-}

-- cases 1-4
balance DoubleBlack (Tree Red a xk xv (Tree Red b yk yv c)) zk zv d =
  Tree Black (Tree Black a xk xv b) yk yv (Tree Black c zk zv d)
balance DoubleBlack (Tree Red (Tree Red a xk xv b) yk yv c) zk zv d =
  Tree Black (Tree Black a xk xv b) yk yv (Tree Black c zk zv d)
balance DoubleBlack a xk xv (Tree Red (Tree Red b yk yv c) zk zv d) =
  Tree Black (Tree Black a xk xv b) yk yv (Tree Black c zk zv d)
balance DoubleBlack a xk xv (Tree Red b yk yv (Tree Red c zk zv d)) =
  Tree Black (Tree Black a xk xv b) yk yv (Tree Black c zk zv d)
-- cases 5-6
balance DoubleBlack a xk xv (Tree NegativeBlack (Tree Black b yk yv c) zk zv d@(Tree Black _ _ _ _)) =
  Tree Black (Tree Black a xk xv b) yk yv (balance Black c zk zv (redden d))
balance DoubleBlack (Tree NegativeBlack a@(Tree Black _ _ _ _) xk xv (Tree Black b yk yv c)) zk zv d =
  Tree Black (balance Black (redden a) xk xv b) yk yv (Tree Black c zk zv d)

-- Otherwise, no problem - just give back a tree with the values
balance color a xk xv b = Tree color a xk xv b

{-|
Delete is not trivial, and there are a lot of different approaches we can take. In an imperative setting, we can rebalance the tree through left/right rotations. In a functional setting, there was a paper published a few years ago that demonstrates a simpler way to delete elements by introducing two new temporary colors, NegativeBlack (-1 black) and DoubleBlack (2 black). The idea is to use these colors and then "bubble up" the color to the parent, where it can be rebalanced.

We'll use Matt Might's strategy at http://matt.might.net/articles/red-black-delete/, implemented in Racket. There's a more formal explanation at http://matt.might.net/papers/germane2014deletion.pdf .
-}

get :: Ord k => k -> Dict k v -> Maybe v
get key BlackLeaf = Nothing
get key (Tree _ l k v r)
  | key <  k = get key l
  | key == k = Just v
  | key >  k = get key r

isEmpty :: Dict k v -> Bool
isEmpty BlackLeaf = True
isEmpty _ = False

empty :: Dict k v
empty = BlackLeaf

isMember :: Ord k => k -> Dict k v -> Bool
isMember item BlackLeaf = False
isMember item (Tree _ left k _ right)
  | item <  k = isMember item left
  | item == k = True
  | item >  k = isMember item right

max :: Tree k v -> (k, v)
max BlackLeaf = error "empty tree"
max (Tree _ _ k v BlackLeaf) = (k, v)
max (Tree _ _ k v r) = Dict.max r

size :: Dict k v -> Int
size dict =
  sizeHelp 0 dict
    where sizeHelp n BlackLeaf = n
          sizeHelp n (Tree _ left _ _ right) = sizeHelp (sizeHelp (n+1) right) left

foldl :: Ord k => (k -> v -> b -> b) -> b -> Dict k v -> b
foldl f acc BlackLeaf = acc
foldl f acc (Tree _ l k v r) = Dict.foldl f (f k v (Dict.foldl f acc l)) r

fromList :: Ord k => [(k, v)] -> Dict k v
fromList pairs =
  Data.List.foldl (\dict (k, v) -> Dict.insert k v dict) empty pairs

toList :: Ord k => Tree k v -> [(k, v)]
toList BlackLeaf = []
toList (Tree _ l xk xv r) = (toList l) ++ [(xk, xv)] ++ (toList r)
