reversibleStream :: [Integer]

reversal :: Integer -> Integer
reversal = read . reverse . show

isPrime :: Integer -> Integer -> Bool
isPrime prime i
    | i <= 1 = True
    | prime `mod` i == 0 = False
    | otherwise = isPrime prime (i-1)


isPrimeReversal :: Integer -> Bool
isPrimeReversal n = isPrime n (floor (sqrt (fromIntegral n))) && isPrime (reversal n) (floor (sqrt (fromIntegral (reversal n))))

reversibleStream = filter isPrimeReversal [1..10000]

main :: IO()
main = do print reversibleStream

