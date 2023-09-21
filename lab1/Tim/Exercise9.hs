module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _  = False
isPermutation _  [] = False
isPermutation x y
    | length x /= length y = False
    | x == y = False
    | otherwise = all (\e -> countElem e x == countElem e y) x

countElem :: Eq a => a -> [a] -> Int
countElem _ [] = 0
countElem e (x:xs)
    | e == x = 1 + countElem e xs
    | otherwise = countElem e xs

-- Property: The lists have the same length
prop_sameLength :: ([Int], [Int]) -> Property
prop_sameLength (x, y) = property ((length x) == (length y))

-- Property: The lists exists of common elements only
prop_sameSorted :: ([Int], [Int]) -> Property
prop_sameSorted(x, y) = property (sort x == sort y)

-- Property: The input list is not a copy of itself
prop_notCopy :: ([Int], [Int]) -> Property
prop_notCopy (x, y) = property (x /= y)

-- Property: A list has only 1 element to be a permutation
prop_notOneElementList :: ([Int], [Int]) -> Property
prop_notOneElementList (x, y) = property (((length x) /= 1) && ((length y) /= 1))

-- Property: A list can't consist of all the same elements to be a permutation
prop_notAllSameElements :: ([Int], [Int]) -> Property
prop_notAllSameElements (x, y) = property (((length (nub x)) == (length x)) && ((length (nub y)) == (length y)))

-- Property: The second list is a permutation of the first list
prop_isPermutationOf :: ([Int], [Int]) -> Property
prop_isPermutationOf (x, y) = property (isPermutation x y)

genPermutations :: Gen ([Int], [Int])
genPermutations = do
  n <- choose (2, 25)
  originalList <- shuffle [1..n]
  permutation <- shuffle originalList
  if originalList == permutation
    then genPermutations
    else return (originalList, permutation)

-- main :: IO ()
-- main = do
--   quickCheck (forAll genPermutations prop_sameLength)
--   quickCheck (forAll genPermutations prop_sameSorted)
--   quickCheck (forAll genPermutations prop_notCopy)
--   quickCheck (forAll genPermutations prop_notOneElementList)
--   quickCheck (forAll genPermutations prop_notAllSameElements)
--   quickCheck (forAll genPermutations prop_isPermutationOf)

main :: IO ()
main = sample genPermutations