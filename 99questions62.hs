{-
4 Problem 62
Collect the internal nodes of a binary tree in a list

An internal node of a binary tree has either one or two non-empty successors. Write a predicate internals/2 to collect them in a list.

Example:

% internals(T,S) :- S is the list of internal nodes of the binary tree T.
Example in Haskell:

Prelude>internals tree4
Prelude>[1,2]
-}

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

internals :: Tree a -> [a]
internals Empty = []
internals (Branch a Empty Empty) = []
--internals (Branch a l r) = [a] ++ internals l ++ internals r
internals (Branch a l r) = a : internals l ++ internals r

