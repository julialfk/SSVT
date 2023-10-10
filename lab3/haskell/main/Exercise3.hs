module Exercise3 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Exercise4

-- Time spent: 8 hrs

-- To calculate the minimal property subsets, you would need to know which
-- properties are equivalent.
-- You could do this by checking the strength of the full set and comparing
-- the subsets from smallest to largest until a subset with the same strength
-- is found.
-- For this, the propertySetStrength from exercise 4 is used to calculate strength.

-- This method follows the one described in the FitSpec document.
-- Another method could compare all properties and their outputs with each other,
-- but I thought that method would be much more tedious to implement.
-- In hindsight, I think that method would actually be simpler to implement.

-- Admittedly, this is not the perfect solution, since the mutations created to
-- calculate the strength are not deterministically created, so there might be
-- variation in strength, even when the same set is checked twice.
-- Due to this variation, the strength scores of the total set might never be
-- exactly the same as any subset.
-- A workaround for this would be making either the mutation process deterministic,
-- or edit the countSurvivors function from Ex2 to take a fixed set of mutations as input.

minPropSubsets :: Int -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO [[[Integer] -> Integer -> Bool]]
minPropSubsets nMutants setProps func = do
    let subsetsProps  = groupLists (sortBy (\x y -> compare (length x) (length y)) (init (tail (subsequences setProps)))) []
    totalStrength <- propertySetStrength nMutants setProps func
    findMinSubsets nMutants func totalStrength subsetsProps

-- Separates a list of lists sorted by length into groups of lists with the same length.
-- This function makes it easier to check subsets in batches based on length, so that the
-- process will not terminate after finding the first minimal subset and keeps checking
-- until all subsets of the same length with the max strength are found.
groupLists :: [[a]] -> [[[a]]] -> [[[a]]]
groupLists [] result = result
groupLists orig result = groupLists rest (result ++ [newGroup])
    where
         newGroup = takeWhile (\x -> length x == length (head orig)) orig
         rest = drop (length newGroup) orig

-- Recursively checks the groups of subsets and compares their strength to the strength of
-- the total property set. The recursion stops when the list of subsets with similar strength
-- is not empty (or when the end is reached).
findMinSubsets :: Int -> (Integer -> [Integer]) -> Float -> [[[[Integer] -> Integer -> Bool]]] -> IO [[[Integer] -> Integer -> Bool]]
findMinSubsets _ _ _ [] = return []
findMinSubsets nMutants func totalStrength (s:subsetsProps) = do
    minSubsets <- findSubsets nMutants func totalStrength s []
    if null minSubsets
        then findMinSubsets nMutants func totalStrength subsetsProps
        else return minSubsets

-- A recursive filter function that keeps all subsets in a list of subsets that have the same or
-- a higher strength than the total set.
-- As was stated above, the measured strength is not deterministic. To minimize false negatives,
-- subsets that are greater than the totalStrength are also returned.
findSubsets :: Int -> (Integer -> [Integer]) -> Float -> [[[Integer] -> Integer -> Bool]] -> [[[Integer] -> Integer -> Bool]] -> IO [[[Integer] -> Integer -> Bool]]
findSubsets _ _ _ [] outputSubsets = do return outputSubsets
findSubsets nMutants func totalStrength (i:inputSubsets) outputSubsets = do
    subsetStrength <- propertySetStrength nMutants i func
    if totalStrength >= subsetStrength
        then findSubsets nMutants func totalStrength inputSubsets outputSubsets
        else findSubsets nMutants func totalStrength inputSubsets (i:outputSubsets)

-- Simple test main to check if everything runs.
-- Unfortunately, the function does not work properly, since the length is always 0 no matter the property list.
-- I did not have enough time to find the bug and fix it, but at least the function runs.
main3 :: IO Int
main3 = do
    minsub <- minPropSubsets 4000 [prop_tenElements, prop_tenElements, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero] multiplicationTable
    return (length minsub)