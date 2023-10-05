module Exercise6 where

import Data.List
import Test.QuickCheck
import Mutation
import Text.Printf
import Exercise2
import qualified Exercise4 as Ex4
import MultiplicationTable

-- Time spent so far: 1 hour

-- Create a function that we pass the function under test, a set of properties, and a number
-- indicating the number of mutants, that visualizes the results of the functions from the
-- previous exercises.

-- We took the output of FitSpec (see below) as a framework, and tried to stay as close to it as possible.
-- Text.Printf is amazing.

main :: IO ()
main = do
    resultVisualiser 4000 multiplicationTableProps multiplicationTable


printListItems :: Int -> [a] -> IO ()
printListItems _ [] =
    printf"\n"
printListItems 1 (x:xs) = do
    printf "1"
    printListItems (2) xs
printListItems index [x] = do
    printf " and %d" index
    printListItems (index + 1) []
printListItems index (x:xs) = do
    printf", %d" index
    printListItems (index + 1) xs

resultVisualiser :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> IO ()
resultVisualiser totalNumberOfMutants listOfProperties functionUnderTest = do
    survivors <- countSurvivors totalNumberOfMutants listOfProperties functionUnderTest
    strength <- Ex4.propertySetStrength totalNumberOfMutants listOfProperties functionUnderTest
    printf"Apparent "
    if survivors == 0 then
            printf "complete (..minimal..) "
        else
            printf "incomplete (..minimal..) "
    printf "specification based on\n"
    printf "%d test cases for each of properties " totalNumberOfMutants
    printListItems 1 listOfProperties
    printf "for each of %d mutant variations.\n\n" totalNumberOfMutants
    printf "%d survivors (%.2f%% killed).\n\n" survivors (strength * 100) 
    printf "apparent minimal property subsets: \n"
    printf "conjectures: \n"



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
