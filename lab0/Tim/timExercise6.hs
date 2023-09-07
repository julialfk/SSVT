consecutive101Prime :: Integer

isPrime :: Integer -> Integer -> Bool
isPrime prime i
    | i <= 1 = True
    | prime `mod` i == 0 = False
    | otherwise = isPrime prime (i-1)

isPrimeSmart :: Integer -> Bool
isPrimeSmart n = isPrime n (floor (sqrt (fromIntegral n)))

primes101 :: Integer -> [Integer] -> [Integer]
primes101 n list
    | length list >= 102 = list
    | isPrimeSmart n = primes101 (n+1) (n:list)
    | otherwise = primes101 (n+1) list

consecutive101Prime = sum (primes101 1 [])-1

-- This is the smallest prime number that can be made, because
-- it starts from the smallest number and checks if every
-- successor is a prime, if so it will be added. It ensures
-- there is not a smaller number that can be made of 101
-- prime numbers.
