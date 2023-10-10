module Exercise5 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import System.Random
import Control.Monad (forM)

-- Implement function(s) that calculate the conjectures: properties that are equivalent, whose
-- cases are subsets of other properties, etc.

-- Input: listOfProperties, numberOfMutants, functionUnderTest
-- Output: a list of which each element is a list of properties. For the list of properties: properties that are equivalent are in the
-- same list. Properties that are not equivalent to any other property are in a singleton list.
-- Properties are equivalent when they kill the exact same mutants, and when the exact same mutants survive under both properties.
mutateEquivalence :: (([Integer] -> Integer -> Bool), ([Integer] -> Integer -> Bool)) -> ([Integer] -> Gen [Integer]) -> (Integer -> [Integer]) -> Gen Bool
mutateEquivalence (prop1, prop2) mutate f = do
    input <- choose(0,100)
    result1 <- mutate' mutate [prop1] f input
    result2 <- mutate' mutate [prop2] f input
    return (result1 == result2)

propertyEquivalenceTester :: ([Integer] -> Integer -> Bool) -> [([Integer] -> Integer -> Bool)] -> [([Integer] -> Gen [Integer])] -> (Integer -> [Integer]) -> [Gen Bool]
propertyEquivalenceTester prop1 props mutations f = [mutateEquivalence (prop1, prop2) mutate f | prop2 <- props, mutate <- mutations]

main :: IO ()
main = do
    let props = multiplicationTableProps
    let mutations = mutators

    results <- forM (zip [1..] props) $ \(index, prop) -> do
        putStrLn $ "Now testing property " ++ show index
        let tests = propertyEquivalenceTester prop multiplicationTableProps mutations multiplicationTable
        forM tests $ \test -> generate test

    print results