{-
5 Problem 25
Generate a random permutation of the elements of a list.

Example:

* (rnd-permu '(a b c d e f))
(B A D C E F)
Example in Haskell:

Prelude System.Random>rnd_permu "abcdef"
Prelude System.Random>"badcef"
-}
import System.Random
import Data.List(nub)
rnd_permu :: (Eq a) => [a] -> IO [a]
rnd_permu xs = do 
  gen <- getStdGen
  -- Plural variant of randomR, producing an infinite list of random values instead of returning a new generator.
  -- nub would remove any duplication
  -- without nub, this code can not work as expected
  return $ take (length xs) $ nub $ [xs !! x | x <- randomRs (0, (length xs)-1) gen]
