import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

-- Iteratively sums up the first 101 items in the primes list and
-- drops the first item if the sum is not a prime.
addPrimes :: [Integer] -> Integer
addPrimes ps | prime n = n
             | otherwise = addPrimes (drop 1 ps)
    where n = sum (take 101 ps)

consecutive101Prime :: Integer
consecutive101Prime = addPrimes primes

-- The answers does not have to be checked, since every possible sum
-- before it was already checked. Therefore the answer is guaranteed
-- to be the smallest sum of 101 primes that is also a prime.

-- time spent: 0.5 hrs
