import Debug.Trace
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
-- null is a short circuit, find anyting k `mod` x > 0, then it would stop immediately
-- improve the performance
isPrime k = null  [ x | x <- [2..isqrt k], k `mod` x == 0 ] 
  where isqrt :: Int -> Int
        isqrt = floor . sqrt . fromIntegral

{-
(**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.

Example:

* (gcd 36 63)
9
Example in Haskell:

[myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
[9,3,3]
-}
mygcd :: Int -> Int -> Int
mygcd a 0 = abs a
mygcd a b = mygcd b  (a `mod` b) 

mygcd1 :: Int -> Int -> Int
mygcd1 a b
  | b == 0 = a
  | otherwise = mygcd1 b (a `mod` b)



{-
4 Problem 33
(*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

Example:

* (coprime 35 64)
T
Example in Haskell:

* coprime 35 64
True
-}

coprime :: Int -> Int -> Bool
coprime a b 
  | mygcd a b == 1 = True
  | otherwise = False

{-
5 Problem 34
(**) Calculate Euler's totient function phi(m).

Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

Example:

* (totient-phi 10)
4
Example in Haskell:

* totient 10
4
-}

totient :: Int -> Int
totient a = helper a (a-1)
  where helper a 1 = 1
        helper a b 
          | coprime a b == True = trace("True a : " ++ show a ++ " b : " ++ show (b-1)) $ (1 + helper a (b-1))
          | otherwise = trace("False a : " ++ show a ++ " b : " ++ show (b-1)) $ helper a (b-1)


totient1 :: Int -> Int
totient1 a = length [x|x <- [1..a-1] , coprime a x]

totient2 :: Int -> Int
totient2 a = length $ filter (coprime a) [1..a-1]
