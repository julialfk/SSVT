module Exercise4 where

import Data.List
import Test.QuickCheck
import Mutation
import qualified Exercise2 as Ex2
import MultiplicationTable

main :: IO ()
main = do
    result <- propertySetStrength 4000 multiplicationTableProps multiplicationTable
    print result

-- Time spent: 15 minutes

-- Implement a function that calculates the strength of a given set of properties,
-- which is the percentage of mutants they kill.

-- so: (total number of mutants - survivors) / total number of mutants

-- is the total number of mutants arbitrary? Can I make this up myself?
-- I suspect that there is a minimum amount for the ratio to actually say something
-- We could set it to, say, 4000 always. Or we could pick a random number between, say, 3000 and 5000

propertySetStrength :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> IO Float
propertySetStrength totalNumberOfMutants listOfProperties functionUnderTest = do
    survivors <- Ex2.countSurvivors totalNumberOfMutants listOfProperties functionUnderTest
    return (fromIntegral (totalNumberOfMutants - survivors) / fromIntegral totalNumberOfMutants)
