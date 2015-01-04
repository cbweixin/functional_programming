{-
Binary trees
A binary tree is either empty or it is composed of a root element and two successors, which are binary trees themselves.

p67.gif

In Haskell, we can characterize binary trees with a datatype definition:

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
This says that a Tree of type a consists of either an Empty node, or a Branch containing one value of type a with exactly two subtrees of type a.

Given this definition, the tree in the diagram above would be represented as:

tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))
Since a "leaf" node is a branch with two empty subtrees, it can be useful to define a shorthand function:

leaf x = Branch x Empty Empty
Then the tree diagram above could be expressed more simply as:

tree1' = Branch 'a' (Branch 'b' (leaf 'd')
                                (leaf 'e'))
                    (Branch 'c' Empty
                                (Branch 'f' (leaf 'g')
                                            Empty)))
Other examples of binary trees:

-- A binary tree consisting of a root node only
tree2 = Branch 'a' Empty Empty
 
-- An empty binary tree
tree3 = Empty
 
-- A tree of integers
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)
-} 

{-
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))
-}

{-
3 Problem 55
(**) Construct completely balanced binary trees

In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.

Example:

* cbal-tree(4,T).
T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
etc......No
Example in Haskell, whitespace and "comment diagrams" added for clarity and exposition:

*Main> cbalTree 4
[
-- permutation 1
--     x
--    / \
--   x   x
--        \
--         x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)),
 
-- permutation 2
--     x
--    / \
--   x   x
--      /
--     x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty),
 
-- permutation 3
--     x
--    / \
--   x   x
--    \
--     x
Branch 'x' (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)) 
           (Branch 'x' Empty Empty),
 
-- permutation 4
--     x
--    / \
--   x   x
--  /
-- x
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty) 
           (Branch 'x' Empty Empty)
]
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree1 :: Int -> [Tree Char]
cbalTree1 0 = [Empty]
cbalTree1 n = let (q, r) = (n-1) `quotRem` 2 
  in [Branch 'x' left right | i  <- [q..q+r],
                              left <- cbalTree1 i,
                              right <- cbalTree1 (n-i-1)]


