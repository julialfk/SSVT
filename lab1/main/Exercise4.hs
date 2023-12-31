module Exercise4 where
import Data.List
import System.Random
import Test.QuickCheck

-- Checks if list is a derangement
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] _  = False
isDerangement _  [] = False
isDerangement xs ys
    | length xs /= length ys = False
    | xs == ys               = False
    | otherwise              = all (\(a, b) -> a /= b) (zip xs ys)

-- Creates derangements of a list
derange :: Eq a => [a] -> [[a]]
derange [] = [[]]
derange xs = [perm | perm <- permutations xs, isDerangement xs perm]

-- Generates all derangements of numbers from 0 to n-1
deran :: Int -> [[Int]]
deran n = derange [0..n-1]

-- Property: The lists must have the same length
-- We use this property because it holds true to all derangements
prop_sameLength :: ([Int], [Int]) -> Property
prop_sameLength (x, y) = property ((length x) == (length y))

-- Property: The lists are the same if it is sorted
-- We use this property because it holds true to all derangements
prop_sameSorted :: ([Int], [Int]) -> Property
prop_sameSorted(x, y) = property (sort x == sort y)

-- Property: The input list is not a copy of itself
-- We use this property because it holds true to all derangements (except empty list)
prop_notCopy :: ([Int], [Int]) -> Property
prop_notCopy (x, y) = (length (x) > 0 && length (y) > 0) ==> property (x /= y)

-- Property: A list of one element is not a derangement
-- We use this property because it holds true to all derangements
prop_notSingleton :: ([Int], [Int]) -> Property
prop_notSingleton (x, y) = property (isDerangement x y == False)

-- Property: A list can't consist of only duplicate elements
-- We use this property because it holds true to all derangements
prop_notAllDuplicates :: ([Int], [Int]) -> Property
prop_notAllDuplicates (x, y) = property (isDerangement x y == False)

-- Property: The second list is a derangement of the first list
-- This property is the final test, to test if our function indeed works
prop_isDerangementOf :: ([Int], [Int]) -> Property
prop_isDerangementOf (x, y) = property (isDerangement x y)

-- Generator: Generator to create lists with only duplicate elements
-- We use this generator to generate duplicate lists for the specific property
genDuplicates :: Gen ([Int], [Int])
genDuplicates = do
  n <- choose (2, 25)
  x <- arbitrary
  return ((replicate n x), (replicate n x))

-- Generator: Generator to create lists with single elements
-- We use this generator to generate singleton lists for the specific property
genSingleton :: Gen ([Int], [Int])
genSingleton = do
  x <- arbitrary
  return ([x], [x])

-- Generator: Generator to create list and it's reversal (which is a derangement)
-- Not all derangement are reversals, but on even lists every reversal is a derangement
-- thus this is an easy way to generate derangements.
genReverses :: Gen ([Int], [Int])
genReverses = do
  n <- arbitrary `suchThat` (\n -> n `mod` 2 == 0 && n <= 100 && n >= 2) -- needs to be even to be derangement
  originalList <- shuffle [1..n]
  let derangedList = reverse originalList
  return (originalList, derangedList)

-- The tests are ordered by strength
-- The strongest test is notSingleton and weakest isDerangementOf
main :: IO ()
main = do
  quickCheck (forAll genSingleton prop_notSingleton)
  quickCheck (forAll genDuplicates prop_notAllDuplicates)
  quickCheck (forAll genReverses prop_notCopy)
  quickCheck (forAll genReverses prop_sameLength)
  quickCheck (forAll genReverses prop_sameSorted)
  quickCheck (forAll genReverses prop_isDerangementOf)

-- The tests are mostly done by using a reversal, which is (when even)
-- a derangement of an original list.

-- All tests succeeded, all singleton lists weren't derangements,
-- the lists that consisted of all duplicate elements weren't derangements,
-- the non-empty lists weren't a copy of itself, the lists did have the same length,
-- and were the same when sorted. The last test tests if the isDerangement function works
-- properly.


-- This exercise took us 5 hours
