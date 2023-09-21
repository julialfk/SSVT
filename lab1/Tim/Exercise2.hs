module Exercise2 where
import Data.List
import System.Random
import Test.QuickCheck


subseq :: [a] -> [[a]]
subseq [] = [[]]
subseq (x:xs) = subseq xs ++ map (x:) (subseq xs)

-- Generate lists with limited range, because list growth is O(2^n)
genList :: Gen [Int]
genList = do
  n <- choose (0, 25)
  vectorOf n arbitrary

-- Compare length of subsequences to formula
prop_length :: [Int] -> Property
prop_length l = length (subseq l) === ( 2^ length(l))

main :: IO ()
main = do
  quickCheck $ forAll genList prop_length


-- 100 too much

-- 50 also very slow

-- 25 a bit quicker, but still very slow


-- Property: The lists have the same length
prop_sameLength :: [Int] -> [Int] -> Property
prop_sameLength x y = property ((length x) == (length y))

-- Property: The lists exists of common elements only
prop_commonElementsOnly :: [Int] -> [Int] -> Property
prop_commonElementsOnly x y = property (null (x `intersect` y))

-- Property: The input list is not a copy of itself
prop_notCopy :: [Int] -> [Int] -> Property
prop_notCopy x y = property (x /= y)

-- Property: A list can't be empty or have only 1 element to be a derangement
prop_notOneElementList :: [Int] -> [Int] -> Property
prop_notOneElementList x y = property (((length x) >= 2) && ((length y) >= 2))

-- Property: A list can't consist of all the same elements to be a derangement
prop_notAllSameElements :: [Int] -> [Int] -> Property
prop_notAllSameElements x y = property (((length (nub x)) == (length x)) && ((length (nub y)) == (length y)))