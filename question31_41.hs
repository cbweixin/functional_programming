{-
2 Problem 31
(**) Determine whether a given integer number is prime.

Example:

* (is-prime 7)
T
Example in Haskell:

P31> isPrime 7
True
-}

isPrime :: Int -> Bool
isPrime k = null  [ x | x <- [2..isqrt k], k `mod` x == 0 ] 
  where isqrt :: Int -> Int
        isqrt = floor . sqrt . fromIntegral
