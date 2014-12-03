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

{-
6 Problem 35
(**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.

Example:

* (prime-factors 315)
(3 3 5 7)
Example in Haskell:

> primeFactors 315
[3, 3, 5, 7]
-}

primeFactors :: Int -> [Int]
--primeFactors a = filter isPrime [x|x<-[3..a-1], a `mod` x == 0] 
-- please notice the let .. in expression indentation
-- f' = must be indented with (f,f1) =
-- it should be looked like 
-- let a = 
--     b = 
--     c=
-- in. .    
primeFactors a = let (f, f1) = factorPairOf a
                     f' = [f] 
                     f1' = if isPrime f1 then [f1] else primeFactors f1
                 in f' ++ f1'
   -- where is a clause of let, it could not be aligned with 'p' in 'primeFactors'
   where 
   factorPairOf a = let f = head $ factors $ a 
                       in (f, a `div` f)
   factors a = filter isPrime [x|x <- [3..a-1], a `mod` x == 0]


-- we can using explict structure instead indentation block
-- by using curly brackt { and semicolon ;
primeFactors1 :: Int -> [Int]
primeFactors1 a = let { (f, f1) = factorPairOf a;
                          f' = [f]; 
                          f1' = if isPrime f1 then [f1] else primeFactors f1;
                      }
                 in f' ++ f1'
   -- where is a clause of let, it could not be aligned with 'p' in 'primeFactors'
   where 
   factorPairOf a = let f = head $ factors $ a 
                       in (f, a `div` f)
   factors a = filter isPrime [x|x <- [3..a-1], a `mod` x == 0]

{-
Problem 36
(**) Determine the prime factors of a given positive integer.

Construct a list containing the prime factors and their multiplicity.

Example:

* (prime-factors-mult 315)
((3 2) (5 1) (7 1))
Example in Haskell:

*Main> prime_factors_mult 315
[(3,2),(5,1),(7,1)]
-}
packList :: [Int] -> [(Int,Int)]
packList = foldr helper [] 
  where helper x [] = [(x ,1)] 
        helper x (y@(a,b):ys)
               | x == a = (a,1+b) : ys  
               | otherwise = (x,1) : y : ys

prime_factors_mult :: Int -> [(Int, Int)] 
prime_factors_mult  = packList . primeFactors 
{-
8 Problem 37
(**) Calculate Euler's totient function phi(m) (improved).

See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:

phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
         (p2 - 1) * p2 ** (m2 - 1) * 
         (p3 - 1) * p3 ** (m3 - 1) * ...
Note that a ** b stands for the b'th power of a.
-}
