module Exercise1and2 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Data.Maybe
import Control.Monad (liftM)

-- Replace nth element in list
replace :: [a] -> (Int, a) -> [a]
replace xs (i, e) = before ++ [e] ++ after
  where
    (before, _:after) = splitAt i xs

-- Mutate a list by changing one element
changeElement :: [Integer] -> Gen [Integer]
changeElement xs = do
  num <- arbitrary :: Gen Integer
  index <- choose (0, length xs - 1)
  return (replace xs (index, num))

-- Return an empty list
emptyList :: [a] -> Gen [b]
emptyList _ = return []

-- Return an integer instead of list
notAList :: [Int] -> Gen Int
notAList xs = do
    num <- arbitrary :: Gen Int
    return num

-- Change a list of ints to a list of chars
charList :: [Int] -> Gen [Char]
charList xs = do
    chars <- arbitrary :: Gen [Char]
    return chars

-- Assuming countSurvivors returns IO [Int]
countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> IO Int
countSurvivors n xs f = do
    survivors <- generate $ vectorOf 4000 (getSurvived xs f)
    return (sum survivors)

getSurvived :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Int
getSurvived xs f = do
    result <- mutateFunction xs f
    points <- isKilled result
    return points

isKilled :: Bool -> Gen Int
isKilled action = do
    -- result <- action
    return $ if action then 1 else 0

mutateFunction :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Bool
mutateFunction xs f = do
    let input = 10
    let mutation = addElements -- [addElements, removeElements, anyList]
    result <- mutate' mutation xs f input
    return (all (== True) result)


-- mutateFunction :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer])

-- testAllProps :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Bool
-- testAllProps [] _ = True
-- testAllProps (property:xs) f = do
--     let input = 1 -- choose(1,100)
--     let mutation = addElements -- elements[addElements, removeElements, anyList]
--     let result = generate (mutate' mutation [property] f input)
--     case result of 
--         [True] -> testAllProps xs f
--         [False] -> False
--         [] -> testAllProps xs f

-- Define a property to test using mutate
-- myProperty :: Integer -> Property
-- myProperty input =
--   forAll (mutate arbitraryMutator arbitraryProperty arbitraryFut input) $ \result ->
--     case result of
--       Just boolResult -> classify boolResult "Success" $ boolResult === True
--       Nothing -> classify True "Mutation failed" False




main :: IO ()
main = do
  -- Generate and print 5 samples
    generatedSample <- generate (charList [1,2,3,4])

    print generatedSample