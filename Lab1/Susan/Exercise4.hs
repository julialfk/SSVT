import System.Random
import Test.QuickCheck
import Data.List

main :: IO ()
main = do
    quickCheck (forAll genDerangements prop_Reverse)

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] _ = False
isDerangement _ [] = False
isDerangement (x:xs) (y:ys) | x == y = False
                            | otherwise = isDerangement xs ys

deran:: Int -> [[Int]] 
deran n =  [permutation | permutation <- permutations [0..n-1], isDerangement permutation [0..n-1]]


prop_Reverse :: [a] -> Property
prop_Reverse xs = property (isDerangement xs (reverse xs))

prop_OneElement :: [a] -> Property
prop_OneElement xs = property (isDerangement xs == False)

-- genSingletonList :: Gen [Int]
-- genSingletonList = do
--   x <- arbitraty
--   return [x]

genDerangements :: Gen [Int]
genDerangements = do
  n <- arbitrary `suchThat` (\n -> n `mod` 2 == 0 && n <= 100) --choose (2, 25)
  originalList <- shuffle [1..n]
  return originalList
