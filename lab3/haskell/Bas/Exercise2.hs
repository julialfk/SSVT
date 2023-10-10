module Exercise2 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Data.Maybe

-- Counts the surviving mutations that will not fail the properties
countSurvivors :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> IO Integer
countSurvivors n xs f = do
    survivors <- generate $ vectorOf n (getSurvived xs f)
    return (sum survivors)

-- Tests if a result is killed when mutating the output and looking at different properties.
getSurvived :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Int
getSurvived xs f = do
  result <- mutateFunction xs f
  isKilled result

-- Return 1 if a mutation survived, 0 if not.
isKilled :: Bool -> Gen Int
isKilled action = do
    return $ if action then 1 else 0

-- Chooses an input for the function, chooses how to mutate the output, takes the results of mutate' to see if
-- all properties came back positive using this mutation. Returns False if not or if nothing was changed by
-- mutation
mutateFunction :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Bool
mutateFunction xs f = do
    input <- choose(0,100)
    mutation <- elements [addElements, removeElements, anyList]
    result <- mutate' mutation xs f input
    if length result == 0 then return False else return (all (== True) result)

-- Time spent: 
-- Tim: 6 hours
-- Bas: 8 hours
-- We got stuck on unpacking gen/io bools for a long time. Eventually we decided not to unpack them.