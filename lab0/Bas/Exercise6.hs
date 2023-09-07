consecutive101Prime :: Integer
consecutive101Prime = getSmallest101Prime 0

getSmallest101Prime :: Int -> Integer
getSmallest101Prime x = if isPrime (sumList (get101Primes x)) then sumList (get101Primes x) else getSmallest101Prime (x + 1)

get101Primes :: Int -> [Integer]
get101Primes n = take 101 $ drop n [x | x <- [2..], isPrime x]

isPrime :: Integer -> Bool
isPrime x = all (\d -> x `mod` d /= 0) [2..x-1]

sumList :: [Integer] -> Integer
sumList ls = foldr (+) 0 ls


