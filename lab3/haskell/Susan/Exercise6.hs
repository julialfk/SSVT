import Data.List
import Test.QuickCheck
import Mutation
-- import Exercise1and2
-- import Exercise4

-- Create a function that we pass the function under test, a set of properties, and a number
-- indicating the number of mutants, that visualizes the results of the functions from the
-- previous exercises.

-- resultVisualiser :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> IO ()
-- resultVisualiser totalNumberOfMutants listOfProperties functionUnderTest = do
--     let survivors = countSurvivors totalNumberOfMutants listOfProperties functionUnderTest
--     let strength = propertySetStrength totalNumberOfMutants listOfProperties functionUnderTest
--     putStrLn ("Tested the given function with " ++ totalNumberOfMutants ++ " mutants.")
--     putStrLn (survivors ++ " survivors (" ++ strength ++ "% killed).")

-- resultVisualiser :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> IO ()
resultVisualiser totalNumberOfMutants listOfProperties functionUnderTest = do
    let strength = fromIntegral (totalNumberOfMutants - 300) / fromIntegral totalNumberOfMutants * 100
    putStrLn ("Tested the given function with " ++ show totalNumberOfMutants ++ " mutants.")
    putStrLn ("300" ++ " survivors (" ++ show strength ++ "% killed).")

-- ghci> testFitSpec
-- Apparent complete but non-minimal specification based on
-- 4000 test cases for each of properties 1, 2, 3, 4 and 5
-- for each of 4000 mutant variations.

-- 0 survivors (100% killed).

-- apparent minimal property subsets:  {1}
-- conjectures:  {3}   ==> {5}     96% killed (weak)
--               {2,4} ==> {5}     99% killed (weak)
--               {3,4} ==> {2}     99% killed (weak)
