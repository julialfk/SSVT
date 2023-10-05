module Exercise5 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import System.Random

-- Implement function(s) that calculate the conjectures: properties that are equivalent, whose
-- cases are subsets of other properties, etc.

-- Input: listOfProperties, numberOfMutants, functionUnderTest
-- Output: a list of which each element is a list of properties. For the list of properties: properties that are equivalent are in the
-- same list. Properties that are not equivalent to any other property are in a singleton list.
-- Properties are equivalent when they kill the exact same mutants, and when the exact same mutants survive under both properties.
mutateTwoFunctions :: (([Integer] -> Integer -> Bool), ([Integer] -> Integer -> Bool)) -> (Integer -> [Integer]) -> Gen Bool
mutateTwoFunctions prop1 prop2 f = do
    input <- choose(0,100)
    mutation <- elements [addElements, removeElements, anyList]
    result1 <- mutate' mutation prop1 f input
    result2 <- mutate' mutation prop2 f input
    return result1 == result2

propertyEquivalence :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> [Gen Bool]
propertyEquivalence props f = [mutateTwoFunctions (prop1, prop2) f | prop1 <- props, prop2 <- props]
