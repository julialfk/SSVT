module Exercise1and2 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Data.Maybe
import Control.Monad (liftM)

-- Replace nth element in list
replace :: [a] -> (Int, a) -> [a]
replace xs (i, e) = before ++ [e] ++ after
  where
    (before, _:after) = splitAt i xs

-- Mutate a list by changing one element
changeElement :: [Integer] -> Gen [Integer]
changeElement xs = do
  num <- arbitrary :: Gen Integer
  index <- choose (0, length xs - 1)
  return (replace xs (index, num))

-- Return an empty list
emptyList :: [a] -> Gen [b]
emptyList _ = return []

countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> IO Int
countSurvivors n xs f = do
    survivors <- generate $ vectorOf 4000 (getSurvived xs f)
    return (sum survivors)

getSurvived :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Int
getSurvived xs f = do
    result <- mutateFunction xs f
    points <- isKilled result
    return points

isKilled :: Bool -> Gen Int
isKilled action = do
    return $ if action then 1 else 0

mutateFunction :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Bool
mutateFunction xs f = do
    input <- choose(0,100)
    mutation <- elements [addElements, removeElements, anyList]
    result <- mutate' mutation xs f input
    if (length result) == 0 then return False else return (all (== True) result)

main :: IO ()
main = do
  -- Generate and print 5 samples
    generatedSample <- generate (charList [1,2,3,4])

    print generatedSample