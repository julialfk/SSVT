module Exercise5 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import System.Random
import Control.Monad (forM)

-- Time spent: 7 hrs

-- This function gets two properties and uses the mutate function on them, it then checks if the results are the same. It returns a Gen Bool.
mutateEquivalence :: (([Integer] -> Integer -> Bool), ([Integer] -> Integer -> Bool)) -> ([Integer] -> Gen [Integer]) -> (Integer -> [Integer]) -> Gen Bool
mutateEquivalence (prop1, prop2) mutate f = do
    input <- choose(0,100)
    result1 <- mutate' mutate [prop1] f input
    result2 <- mutate' mutate [prop2] f input
    return (result1 == result2)

-- This function unpacks the properties and mutations, it then uses the for monad to loop over the properties and
-- mutations. To keep track of the index it uses a zip, and with multiple mutations it tests them all over the properties
-- you can see the results, if all values are True in the terminal under the equation (these are the different mutations being
-- tested on the property equation), then the property is equivalent with the other property that is being tested.
--
-- Unfortunately, it is not working perfectly yet, we think this is due to the generator (we use Gen Bool and need to print the
-- non IO value). We have used the multiplication table example that is provided in the assignment.
propertyEquivalenceTester :: [([Integer] -> Integer -> Bool)] -> [([Integer] -> Gen [Integer])] -> IO [([Integer] -> Integer -> Bool,
                  [Integer] -> Integer -> Bool, [Integer] -> Gen [Integer])]
propertyEquivalenceTester props mutations = do
  let indexedProps = zipWith (\i p -> (i, p)) [1..] props
  let indexedMutations = zipWith (\i m -> (i, m)) [1..] mutations

  results <- sequence
    [ do
        let (propIndex1, prop1) = indexedProps !! i
        let (propIndex2, prop2) = indexedProps !! j
        let (mutationIndex, mutation) = indexedMutations !! k
        putStrLn $ "Testing property " ++ show propIndex1 ++ " with property " ++ show propIndex2 ++ " with mutation " ++ show mutationIndex

        result <- generate (mutateEquivalence (prop1, prop2) mutation multiplicationTable)
        print result
        putStrLn ""

        return (prop1, prop2, mutation)
    | i <- [0..length props - 1], j <- [0..length props - 1], k <- [0..length mutations - 1]
    ]
  return results

-- Main function to run the property equivalence tester
main :: IO ()
main = do
  let props = multiplicationTableProps
  let mutations = mutators
  testResults <- propertyEquivalenceTester props mutations
  putStrLn "Finished"
