module Exercise8 where

import Data.List
import System.Random
-- import Test.QuickCheck
import Lecture2

-- Prime functions taken from Lab 0
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- First of all, the list of all consecutive primes up until pn must be generated.
-- Using takeWhile with the successor of pn to do this.
listPrimes :: Integer -> [Integer]
listPrimes p = takeWhile (< (p + 1)) primes

-- Simple function to calculate a potential counter example given pn.
counterexample :: Integer -> Integer
counterexample p = product (listPrimes p) + 1

-- Now that we have a function that converts primes into a potential counter example,
-- we can map over the list of primes to create tuples containing the consecutive primes
-- and its counter example.
-- After creating the list of tuples, we can filter out all the non-primes.
counterexamples :: [([Integer], Integer)]
counterexamples = filter (\(_, c) -> not (prime c))
                    (map (\p -> (listPrimes p, counterexample p)) primes)

-- This implementation is not the most efficient, since listPrimes is calculated twice
-- for each tuple in counterexamples, but this implementation seemed the most readable to me.
-- To make this more efficient, the function for counterexample can be directly inserted into
-- counterexamples and listPrimes p can be precalculated using a where statement.

-- time spent: 40 mins
