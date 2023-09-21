module Exercise2 where
import Data.List
import System.Random
import Test.QuickCheck

-- Creates subsequences of a list
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subseq xs ++ map (x:) (subseq xs)

-- Generate lists with limited range, because list growth is O(2^n)
genList :: Gen [Int]
genList = do
  n <- choose (0, 25)
  vectorOf n arbitrary

-- Compare length of subsequences to formula
prop_length :: [Int] -> Property
prop_length l = length (subsequences l) === ( 2^ length(l))


-- Do quickCheck on the formula to check if it's true
main :: IO ()
main = do
  quickCheck $ forAll genList prop_length

-- In this test we limited the range of n to 25, 100 seems to be too slow, 50 also.
-- This is because the subsequences grow exponentially, so the time to calculate too.

-- This exercise took us 3 hours
