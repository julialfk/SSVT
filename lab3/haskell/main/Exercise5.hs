module Exercise5 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
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
main :: IO ()
main = do
    let props = multiplicationTableProps
    let mutations = mutators

    results <- forM (zip [1..] props) $ \(index, prop1) -> do
        putStrLn $ "Now testing property " ++ show index
        forM (zip [1..] props) $ \(otherIndex, prop2) -> do
            putStrLn $ "Testing property " ++ show index ++ " with property " ++ show otherIndex
            forM mutations $ \mutate -> do
                result <- generate (mutateEquivalence (prop1, prop2) mutate multiplicationTable)
                print result

    print results
