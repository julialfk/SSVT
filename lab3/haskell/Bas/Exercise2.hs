module Exercise1and2 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Data.Maybe
import Control.Monad (liftM)

-- Counts the surviving mutations that will not fail the properties
countSurvivors :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> IO Int
countSurvivors n xs f = do
    survivors <- generate $ vectorOf n (getSurvived xs f)
    return (sum survivors)

-- Gets the surviving mutations
getSurvived :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Int
getSurvived xs f = do
  result <- mutateFunction xs f
  isKilled result

-- Returns if an mutation is killed, or survived
isKilled :: Bool -> Gen Int
isKilled action = do
    return $ if action then 1 else 0

-- Chooses an input for the function, chooses how to mutate the output, takes the results of mutate' to see if
-- all properties came back positive using this mutation.
mutateFunction :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Bool
mutateFunction xs f = do
    input <- choose(0,100)
    mutation <- elements [addElements, removeElements, anyList]
    result <- mutate' mutation xs f input
    if length result == 0 then return False else return (all (== True) result)

-- Time spent: 
-- Tim: 6 hours
-- Bas: 8 hours
--