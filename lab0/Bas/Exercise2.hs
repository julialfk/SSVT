import System.Random

main :: IO ()
main = do
    number <- probs 10000
    let result = isRandom number
    print result

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

isRandom :: [Float] -> Bool
isRandom ls = isEvenlyDivided result (length ls)
    where result = getRandomResults ls [0,0,0,0]

isEvenlyDivided :: [Int] -> Int -> Bool
isEvenlyDivided [] len = True
isEvenlyDivided (x:xs) len
    | x < (div len 4) - (floor (fromIntegral len * 0.01)) = False
    | x > (div len 4) + (floor (fromIntegral len * 0.01)) = False
    | otherwise = True

getRandomResults :: [Float] -> [Int] -> [Int]
getRandomResults [] ls = ls
getRandomResults (x:xs) (qone:qtwo:qthree:qfour:rs) = 
    case quartile of
        quartile | quartile == 1 -> getRandomResults xs (qone + 1:qtwo:qthree:qfour:rs)
                 | quartile == 2 -> getRandomResults xs (qone:qtwo + 1:qthree:qfour:rs)
                 | quartile == 3 -> getRandomResults xs (qone:qtwo:qthree + 1:qfour:rs)
                 | otherwise -> getRandomResults xs (qone:qtwo:qthree:qfour + 1:rs)
    where 
        quartile = getQuartile x


getQuartile :: Float -> Int
getQuartile x =
    case x of
    x   | x < 0.25 -> 1
        | x < 0.5 -> 2
        | x < 0.75 -> 3
        | otherwise -> 4