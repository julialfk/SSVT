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

-- Time spent: 30 minutes

-- "Implement a function that calculates the strength of a given set of properties,
-- which is the percentage of mutants they kill"

-- This means the following: (total number of mutants - survivors) / total number of mutants

-- At first we randomly generated the total number of mutants inside of this function. We changed this in order to be able
-- to integrate this function in other functions introduced later in the assignment.
-- There was a thought process here about what number of mutants is a good number. FitSpec has a default number of mutants of 4000.
-- This felt reasonable. The number shoulnd't be too low, because then it might be too hard to find a surviving mutant.
-- The number also shouldn't be too high, because then the computation takes too long.
-- In the old implementation, we took a random number in the range (3500, 4500). This was still somewhat arbitrary, but a
-- reasonable solution. We will stick to the 4000 mutants in the future, because this corresponds to the FitSpec research paper and
-- we trust their judgement.

propertySetStrength :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> IO Float
propertySetStrength totalNumberOfMutants listOfProperties functionUnderTest = do
    survivors <- Ex2.countSurvivors totalNumberOfMutants listOfProperties functionUnderTest
    return (fromIntegral (totalNumberOfMutants - survivors) / fromIntegral totalNumberOfMutants)
