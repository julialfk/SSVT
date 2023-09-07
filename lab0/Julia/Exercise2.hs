import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

-- Usage: 'check 100' to check 100 different cases of lists with 10000 items.
-- Uses recursion to repeat the process and check if all lists are within allowed deviation.
-- 10000 items and 100 repetitions were chosen as they seemed as sufficiently reliable numbers.
check :: Int -> IO()
check 0 = print True
check x = do
    result <- probs n
    if checkBounds n result then check (x - 1) else print False
        where n = 10000

-- Uses a tuple and recursion to keep count of the number of items within each bracket.
countItems :: [Float] -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
countItems [] ns = ns
countItems (x:xs) (a, b, c, d) | x <= 0.25 = countItems xs (a + 1, b, c, d)
                               | x > 0.25 && x <= 0.5 = countItems xs (a, b + 1, c, d)
                               | x > 0.5 && x <= 0.75 = countItems xs (a, b, c + 1, d)
                               | x > 0.75 = countItems xs (a, b, c, d + 1)

tupleToList :: (Int, Int, Int, Int) -> [Int]
tupleToList (a, b, c, d) = [a, b, c, d]

-- Checks each bucket length whether they are still within the deviation of 5% of the average bucket size.
checkBounds :: Int -> [Float] -> Bool
checkBounds n result = any (\x -> x > round ((fromIntegral n) * 0.25 * 0.95) || x < round ((fromIntegral n) * 0.25 * 1.05)) (tupleToList (countItems result (0,0,0,0)))

-- time spent: 2 hrs
