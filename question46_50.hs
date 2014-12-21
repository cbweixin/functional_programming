{-
 Problem 46
(**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.

A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).

Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

Example:

(table A B (and A (or A B)))
true true true
true fail true
fail true fail
fail fail fail
Example in Haskell:

> table (\a b -> (and' a (or' a b)))
True True True
True False True
False True False
False False False
-}

not' :: Bool -> Bool
not' True = False
not' False = True

and', or', nor', nand', xor', impl', equ' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

nor' a b = not' $ or' a b
nand' a b = not' $ and' a b

xor' True False = True
xor' False True = True
xor' _ _ = False

impl' a b = (not' a) `or'` b

equ' True True = True
equ' False False = True
equ' _ _ = False

infixl 4 `or'`
infixl 6 `and'`

table :: (Bool -> Bool -> Bool) -> IO()
table f = mapM_ putStrLn[show a ++ " " ++ show b ++ " " ++ show ( f a b )
                          | a <- [True, False], b <-[True, False]]

{-
3 Problem 47
(*) Truth tables for logical expressions (2).

Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.

Example:

* (table A B (A and (A or not B)))
true true true
true fail true
fail true fail
fail fail fail
Example in Haskell:

> table2 (\a b -> a `and'` (a `or'` not b))
True True True
True False True
False True False
False False False
-}

table2 :: (Bool -> Bool -> Bool) -> IO()
table2 f = mapM_ putStrLn[show a ++ " " ++ show b ++ " " ++ show ( f a b )
                          | a <- [True, False], b <-[True, False]]
