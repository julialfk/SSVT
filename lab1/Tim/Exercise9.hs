module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck

-- Checks if list is a permutation
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _  = False
isPermutation _  [] = False
isPermutation x y
    | length x /= length y = False
    | x == y = False
    | otherwise = all (\e -> countElements e x == countElements e y) x


-- Counts each element in the list to look if number of occurrences are the same
countElements :: Eq a => a -> [a] -> Int
countElements _ [] = 0
countElements e (x:xs)
    | e == x = 1 + countElements e xs
    | otherwise = countElements e xs

-- Property: The lists must have the same length
prop_sameLength :: ([Int], [Int]) -> Property
prop_sameLength (x, y) = property ((length x) == (length y))

-- Property: The lists are the same if it is sorted
prop_sameSorted :: ([Int], [Int]) -> Property
prop_sameSorted(x, y) = property (sort x == sort y)

-- Property: The input list is not a copy of itself (does not hold true if list is empty)
prop_notCopy :: ([Int], [Int]) -> Property
prop_notCopy (x, y) = (length (x) > 0 && length (y) > 0) ==> property (x /= y)

-- Property: A list of one element is not a derangement
prop_notSingleton :: ([Int], [Int]) -> Property
prop_notSingleton (x, y) = property (isPermutation x y == False)

-- Property: A list can't consist of only duplicate elements
prop_notAllDuplicates :: ([Int], [Int]) -> Property
prop_notAllDuplicates (x, y) = property (isPermutation x y == False)

-- Property: The second list is a permutation of the first list
prop_isPermutationOf :: ([Int], [Int]) -> Property
prop_isPermutationOf (x, y) = property (isPermutation x y)

-- Generator: Generator to create lists with only duplicate elements
genDuplicates :: Gen ([Int], [Int])
genDuplicates = do
  n <- choose (2, 25)
  x <- arbitrary
  return ((replicate n x), (replicate n x))

-- Generator: Generator to create lists with single elements
genSingleton :: Gen ([Int], [Int])
genSingleton = do
  x <- arbitrary
  return ([x], [x])

-- Generator: Generator to create list and a permutation of it
genPermutations :: Gen ([Int], [Int])
genPermutations = do
  n <- choose (2, 25)
  originalList <- shuffle [1..n]
  permutation <- shuffle originalList
  if originalList == permutation
    then genPermutations
    else return (originalList, permutation)

-- The tests are ordered by strength
-- The strongest test is notSingleton and weakest isDerangementOf
main :: IO ()
main = do
  quickCheck (forAll genSingleton prop_notSingleton)
  quickCheck (forAll genDuplicates prop_notAllDuplicates)
  quickCheck (forAll genPermutations prop_notCopy)
  quickCheck (forAll genPermutations prop_sameLength)
  quickCheck (forAll genPermutations prop_sameSorted)
  quickCheck (forAll genPermutations prop_isPermutationOf)

-- This exercise took us 3 hours
