import Test.QuickCheck

-- Reverse an integer from for example 34 to 43.
reversal :: Integer -> Integer
reversal = read . reverse . show

-- Get a list of all prime numbers below 100000, check which of these prime numbers
-- is also a prime number when reversed.
reversibleStream :: [Integer]
reversibleStream = [x | x <- primes, isPrime (reversal x)]
    where 
        primes = getPrimes 10000

-- Get a list of n primes.
getPrimes :: Integer -> [Integer]
getPrimes n = [x | x <-[1..n], isPrime x]

-- Check if a number is a prime.
isPrime :: Integer -> Bool
isPrime x = all (\d -> x `mod` d /= 0) [2..x-1]

-- Check if the reverse of a number is a prime and if the number itself is a prime.
propReversable :: Int -> Property
propReversable x = (x >= 0 && x < length reversibleStream) ==> (isPrime testPrime) && (isPrime (reversal testPrime))
    where 
        testPrime = reversibleStream !! x

-- Check if a number is the same as original when reversed twice.
propReverse :: Integer -> Property
propReverse x = x >= 0 ==> (reversal (reversal x)) == x

-- Check if the reversal of the reversal of the prime numbers in the reversible stream is
-- present in the stream as well.
propReversalSym :: Int -> Property
propReversalSym x = (x >= 0 && x < length reversibleStream) ==> elem (reversal testPrime) reversibleStream
    where 
        testPrime = reversibleStream !! x

-- We added the properties reversal correctness, prime reversibility, and reversal symmetry.
-- The reversal correctness test failed, as numbers that ended in 0 change when reversed.
-- For example 100 becomes 1. All numbers that end in 0 are not prime however, so it won't
-- change the outcome. The other properties succeed.
-- This exercise took us about an hour.