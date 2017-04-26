module NineyNineProblems where

import Data.List
import Data.Maybe

type Node = Int
type Edge = (Node, Node)
type Graph = [Edge]

data Tree a = Empty
            | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))

-- dfs2 Empty = []
-- dfs2 (Branch a l r) = [a] ++ dfs2 l ++ dfs2 r
--
-- bfs Empty = []
-- bfs (Branch a l r) =



bfs x = catMaybes (bfs' [x])
  where bfs' [] = []
        bfs' (Empty : xs) = []
        bfs' xs = map value xs ++ bfs' (xs >>= children)

value Empty = Nothing
value (Branch a _ _) = Just a
children (Branch _ l r) = [l, r]

{- 81. Find all the paths in a graph from a to b -}

{- For acyclic graphs, the idea is that we start at a node a.
If a is equal to our target node, b, then we're done - give back a list of paths
with just this node. Otherwise, we need to walk down the paths of each of a's
adjacent nodes (named 'next') and keep the ones where we end up at b.
-}
paths :: Node -> Node -> Graph -> [[Node]]
paths a b g
  | a == b    = [[b]]
  | otherwise = [a : rest | (x, next) <- g,
                            x == a,
                            rest <- paths next b g]

{- For cyclic graphs
We need to keep track of the nodes we've seen so far so we can filter them out,
so we need to extract the function into a helper.
-}
paths2 :: Node -> Node -> Graph -> [[Node]]
paths2 a b g = helper a b g []
  where
    helper a b g seen
      | a == b    = [[b]]
      | otherwise = [a : rest | (x, next) <- g,
                                x == a,
                                rest <- helper next b g (next:seen),
                                next `notElem` seen]

{-
82. Find all cycles in a graph with a given starting node

It's not a coincidence that this comes after problem 81.
If a node is part of a cycle, and we follow every path from
that node, one of those paths has to end up coming back to the
original starting node. That means we need to walk down the paths
beginning with each of the starting node's adjacent nodes and look
for a path back to the original starting node.
-}

cycles :: Node -> Graph -> [[Node]]
cycles a g = [path | (x, adj) <- g,
                     x == a,
                     path <- paths adj a g]

{-
83. Construct all spanning trees of a graph.

Yeah screw that.
-}

{- 84. Construct the minimal spanning tree given a graph.

The minimal spanning tree of a graph is the set of edges that connect every
node and has the minimum total edge weight. The problem suggests using
Prim's algorithm, which is like Djikstra's algorithm. The edges have to be
weighted, so we need a new datatype.

We need to keep track of the set of nodes we've seen so far, so we'll use a helper.

type Node = Int
type Weight = Int
type Edge = (Node, Node, Weight)
type Graph = [Edge]
-}

-- mst :: Graph -> [Edge]
-- mst g = helper g (head g) [head g]
--   where helper g currentNode visitedNodes =
--         outgoingEdges = [e | e@(n, adj, w) <- g,
--                              n == currentNode]
--         nextEdge = minimumBy (comparing (\(_, _, w) -> w)) outgoingEdges
--         node (a, _, _) = a
--         nodeToAdd = node nextEdge
    -- | null currentNode = []
    -- | otherwise = concat $ mst g next (currentNode : visitedNodes)
    -- where next =

main = print "hey"
