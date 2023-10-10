module Exercise2 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Data.Maybe
import Control.Monad (liftM)

-- To count the surviving mutations n amount of times, the mutate function gets
-- used on n amount of mutations to the output of function f. These mutations
-- get tested on all the properties in the given list of properties.

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

-- Returns 0 if a mutation is killed, 1 if it survived
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
-- We got stuck on unpacking gen/io bools for a long time. Eventually we decided not to unpack them.

-- Results of Properties vs Mutators:
-- prop_tenElements 
--
-- vs addElements
-- Survivors: 0 // Length can't be ten if an element was added
-- vs removeElements
-- Survivors: 0 // Length can't be ten if an element was removed
-- vs anyList
-- Survivors: 120 // By chance, a random list can be of length 10

-- prop_firstElementIsInput 
--
-- vs addElements
-- Survivors: 180 // Fails if element was added to head of list
-- vs removeElements
-- Survivors: 3600 // Only fails if the first element was removed
-- vs anyList
-- err // Fails on empty list

-- prop_sumIsTriangleNumberTimesInput 
--
-- vs addElements 
-- Survivors: 20 // Survives if a 0 was added
-- vs removeElements 
-- Survivors: 0
-- vs anyList
-- Survivors: 0

-- prop_linear 
--
-- vs addElements
-- Survivors: 160
-- vs removeElements
-- Survivors: 3550 
-- vs anyList
-- Survivors: 240

-- prop_moduloIsZero 
--
-- vs addElements
-- Survivors: 1 // By chance an element can be added for which modulo input is still 0
-- vs removeElements
-- Survivors: 3600
-- vs anyList
-- Survivors: 140 // By chance a list of elements can be created for which each element modulo input is still 0

