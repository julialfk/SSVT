import System.Random
import Data.List

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

quartile1 :: [Float] -> [Float]
quartile2 :: [Float] -> [Float]
quartile3 :: [Float] -> [Float]
quartile4 :: [Float] -> [Float]

quartile1 = filter (\x -> x <= 0.25)
quartile2 = filter (\x -> x > 0.25 && x <= 0.5)
quartile3 = filter (\x -> x > 0.5 && x <= 0.75)
quartile4 = filter (\x -> x > 0.75)


main :: IO ()
main = do
    randFloats <- probs 10000

    let size_1 = length (quartile1 randFloats)
        size_2 = length (quartile2 randFloats)
        size_3 = length (quartile3 randFloats)
        size_4 = length (quartile4 randFloats)

    putStrLn $ "Quartile 1 size: " ++ show size_1
    putStrLn $ "Quartile 2 size: " ++ show size_2
    putStrLn $ "Quartile 3 size: " ++ show size_3
    putStrLn $ "Quartile 4 size: " ++ show size_4

    -- time spend 40 min

-- you can test using central limit theorem, with the average of the four quartiles