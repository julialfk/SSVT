import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

main :: IO ()
main = do
    result <- probs 10000
    print (bucket1 result)
    print (bucket2 result)
    print (bucket3 result)
    print (bucket4 result)
    

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

bucket1 :: [Float] -> Int
bucket1 xs = length [x | x <- xs, x >= 0, x <= 0.25]
bucket2 :: [Float] -> Int
bucket2 xs = length [x | x <- xs, x > 0.25, x <= 0.5]
bucket3 :: [Float] -> Int
bucket3 xs = length [x | x <- xs, x > 0.5, x <= 0.75]
bucket4 :: [Float] -> Int
bucket4 xs = length [x | x <- xs, x > 0.75, x <= 1]