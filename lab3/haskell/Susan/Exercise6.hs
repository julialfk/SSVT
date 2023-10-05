module Exercise6 where

import Data.List
import Test.QuickCheck
import Mutation
import Text.Printf
import Exercise1
import Exercise4

-- Time spent so far: 30 minutes

-- Create a function that we pass the function under test, a set of properties, and a number
-- indicating the number of mutants, that visualizes the results of the functions from the
-- previous exercises.

resultVisualiser :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> IO ()
resultVisualiser totalNumberOfMutants listOfProperties functionUnderTest = do
    let survivors = countSurvivors totalNumberOfMutants listOfProperties functionUnderTest
    let strength = propertySetStrength totalNumberOfMutants listOfProperties functionUnderTest * 100
    printf "Tested the given function with %d mutants.\n" totalNumberOfMutants
    printf "%d survivors (%.3f%% killed).\n" survivors percentageKilled


-- #############################################################
-- VISUALISATION EXAMPLE FROM FITSPEC
-- #############################################################
-- ghci> testFitSpec
-- Apparent complete but non-minimal specification based on
-- 4000 test cases for each of properties 1, 2, 3, 4 and 5
-- for each of 4000 mutant variations.

-- 0 survivors (100% killed).

-- apparent minimal property subsets:  {1}
-- conjectures:  {3}   ==> {5}     96% killed (weak)
--               {2,4} ==> {5}     99% killed (weak)
--               {3,4} ==> {2}     99% killed (weak)
-- #############################################################
