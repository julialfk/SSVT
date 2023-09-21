module Exercise4 where
import Data.List
import System.Random
import Test.QuickCheck

-- Checks if list is indeed a derangement
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] _  = False
isDerangement _  [] = False
isDerangement xs ys
    | length xs /= length ys = False
    | xs == ys               = False
    | otherwise              = all (\(a, b) -> a /= b) (zip xs ys)

derange :: Eq a => [a] -> [[a]]
derange [] = [[]]
derange xs = [perm | perm <- permutations xs, isDerangement xs perm]

-- Generates all derangements of numbers from 0 to n-1
deran :: Int -> [[Int]]
deran n = derange [0..n-1]

genDerangements :: Gen ([Int], [Int])
genDerangements = do
  n <- arbitrary `suchThat` (\n -> n `mod` 2 == 0 && n <= 100 && n >= 2) --choose (2, 25)
  originalList <- shuffle [1..n]
  let derangedList = reverse originalList
  return (originalList, derangedList)

-- Property: The lists have the same length
prop_sameLength :: ([Int], [Int]) -> Property
prop_sameLength (x, y) = property ((length x) == (length y))

-- Property: The lists exists of common elements only
prop_sameSorted :: ([Int], [Int]) -> Property
prop_sameSorted(x, y) = property (sort x == sort y)

-- Property: The input list is not a copy of itself
prop_notCopy :: ([Int], [Int]) -> Property
prop_notCopy (x, y) = (length (x) > 0 && length (y) > 0) ==> property (x /= y)

-- Property: A list has only 1 element to be a permutation
prop_notOneElementList :: ([Int], [Int]) -> Property
prop_notOneElementList (x, y) = property (((length x) /= 1) && ((length y) /= 1))

-- Property: A list can't consist of all the same elements to be a permutation
prop_notAllSameElements :: ([Int], [Int]) -> Property
prop_notAllSameElements (x, y) = property (((length (nub x)) == (length x)) && ((length (nub y)) == (length y)))

-- Property: The second list is a permutation of the first list
prop_isDerangementOf :: ([Int], [Int]) -> Property
prop_isDerangementOf (x, y) = property (isDerangement x y)

-- main :: IO ()
-- main = do
--     sample genDerangements

main :: IO ()
main = do
  quickCheck (forAll genDerangements prop_sameLength)
  quickCheck (forAll genDerangements prop_sameSorted)
  quickCheck (forAll genDerangements prop_notCopy)
  quickCheck (forAll genDerangements prop_notOneElementList)
  quickCheck (forAll genDerangements prop_notAllSameElements)
  quickCheck (forAll genDerangements prop_isDerangementOf)