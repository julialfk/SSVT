module Exercise5 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable

-- Time spent: 7 hrs

-- We set out to find the conjecture of properties being equal to one another.
-- The idea here was to find all the properties in a testing suit (defined as a list of properties) that are equal to each other.
-- Equality would be established if two properties, for all the same mutators and inputs, gave the same result. This result can be
-- False, True, or an empty list (see the documentation of the mutate' function).
-- To find out whether two properties have the same result for one instance of a mutator and input, the 'mutateEquivalence' function is
-- used. To establish equivalence we of course have to test a lot more cases than just one mutator with one input. We have to
-- systematically go over a lot of different input/mutator combinations with the same two properties, and see if they
-- are always the same (mutateEquivalence returns True). So create a list of mutateEquivalence' output, and if all (==True), then these
-- two properties are equal. We have to generate such a list for all the different combinations of two properties that
-- can be formed out of the list of different properties of the testing suit. In the end we could output all the pairs of properties that
-- are equal. We possibly would have to filter our output first, because we can imagine it showing the same pairs twice:
-- once as (prop_1, prop_2) and once as (prop_2, prop_1). This of course, for our purposes, means the same combination of properties.
-- Some other filtering could be done for cases such as (prop_1, prop_2), (prop_1, prop_3), (prop_2, prop_3), in which multiple properties
-- are equal to each other.

-- You can see our attempt for equality below. Please mind that it does not work.

-- This function gets two properties and uses the mutate function on them, it then checks if the results are the same. It returns a Gen Bool.
mutateEquivalence :: (([Integer] -> Integer -> Bool), ([Integer] -> Integer -> Bool)) -> ([Integer] -> Gen [Integer]) -> (Integer -> [Integer]) -> Gen Bool
mutateEquivalence (prop1, prop2) mutator f = do
    input <- choose(0,100)
    result1 <- mutate' mutator [prop1] f input
    result2 <- mutate' mutator [prop2] f input
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
  putStrLn "Finished!"
