{-
2 Problem 61
Count the leaves of a binary tree

A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.

Example:

% count_leaves(T,N) :- the binary tree T has N leaves
Example in Haskell:

> countLeaves tree4
2
-}

import Debug.Trace

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf a = Branch a Empty Empty

countLeaves :: (Show a)=>Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch a Empty Empty) = 1
--countLeaves leaf = trace("haha") 1
countLeaves (Branch a l r) = trace("l has leaves : " ++ show l) $ countLeaves l + countLeaves r

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)
{-
3 Problem 61A
Collect the leaves of a binary tree in a list

A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.

Example:

% leaves(T,S) :- S is the list of all leaves of the binary tree T
Example in Haskell:

> leaves tree4
[4,2]
-}
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch a l r) = leaves l ++ leaves r


