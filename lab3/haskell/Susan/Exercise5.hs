module Exercise5 where

import Exercise2
import Data.List
import Test.QuickCheck
import Mutation
import System.Random

-- Implement function(s) that calculate the conjectures: properties that are equivalent, whose
-- cases are subsets of other properties, etc.

-- Input: listOfProperties, numberOfMutants, functionUnderTest
-- Output: a list of which each element is a list of properties. For the list of properties: properties that are equivalent are in the
-- same list. Properties that are not equivalent to any other property are in a singleton list.
-- Properties are equivalent when they kill the exact same mutants, and when the exact same mutants survive under both properties.
propertyEquivalence :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> [[([Integer] -> Integer -> Property)]]
propertyEquivalence totalNumberOfMutants listOfProperties functionUnderTest = ...?


-- Dit werkt niet; pls lees het niet
equivalentProperties :: ([Integer] -> Integer -> Bool) -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Integer -> Gen Bool
equivalentProperties prop1 prop2 functionUnderTest input = do
    let mutatorList = [anyList, removeElements, addElements]
    randomIndex <- choose (0, length mutatorList - 1)
    let mutateFunction = mutatorList !! randomIndex
    let resultProp1 = mutate' mutateFunction prop1 functionUnderTest input
    let resultProp2 = mutate' mutateFunction prop2 functionUnderTest input
