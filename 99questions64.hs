{-
7 Problem 64
Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid. Several layout methods are conceivable, one of them is shown in the illustration below:

p64.gif

In this layout strategy, the position of a node v is obtained by the following two rules:

x(v) is equal to the position of the node v in the inorder sequence
y(v) is equal to the depth of the node v in the tree
Write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or the rectangle bounding the drawn tree.

Here is the example tree from the above illustration:

Example in Haskell:

> layout tree64
Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...
-}
import Debug.Trace

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

--type Pos = (Int, Int)
--layout :: Tree a -> Tree(a,Pos)
 
layout t =
    helper 8 1 t
    where
      helper x y Empty =
          Empty
      helper x y (Branch _ t0 t1) =
          let
            l = trace("rights t0 : "++ show (rights t0))$rights t0 
            r = trace("lefts t1 : " ++ show(lefts t1))lefts t1
          in
            Branch (x,y) (helper (x - l) (succ y) t0) (helper (x + r) (succ y) t1)
 
      -- counting right steps caused by helper
      rights (Branch _ t0 t1) =
          rightsHelper t1 + 1
      rights Empty = 
          0
      -- counting left steps caused by helper
      lefts (Branch _ t0 t1) =
          leftsHelper t0 + 1
      lefts Empty =
          0
      leftsHelper (Branch _ t0 t1) = trace("rightsHelper t1 : "++ show (rightsHelper t1) ++ " leftsHelper t0 : "++ show (leftsHelper
                       t0))$rightsHelper t1 + leftsHelper t0 + 1
      leftsHelper Empty = 0
      rightsHelper (Branch _ t0 t1) = trace("leftsHelper t0 : "++ show (leftsHelper t0) ++ " rightsHelper t1 : "++ show (rightsHelper 
                       t1))$leftsHelper t0 + rightsHelper t1 + 1
      rightsHelper Empty = 0
