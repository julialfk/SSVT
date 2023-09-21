module Exercise2 where
import System.Random
import Test.QuickCheck

-- Creates subsequences of a list
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

-- Generator: Generate lists with limited range, because list growth is O(2^n)
genList :: Gen [Int]
genList = do
  n <- choose (0, 25)
  vectorOf n arbitrary

-- Property: Compare length of subsequences to formula
-- We use this property to test if the formula holds true for certain input.
prop_length :: [Int] -> Property
prop_length l = length (subsequences l) === ( 2^ length(l))


-- Do quickCheck on the formula to check if it's true
main :: IO ()
main = do
  quickCheck $ forAll genList prop_length

-- The test succeeds, which means that atleast for smaller lists it seems to follow the formula.

-- Question 1: Is the property hard to test? If you find that it is, can you given a reason why?
-- Yes because it takes too much computational time to test bigger lists.

-- Question 2: Give your thoughts on the following issue (see PDF).
-- We are testing single inputs, it is not a mathmatical proof because we can't test until infinity
-- and have not used anything like proof by induction. So we test a part ( | [] | <= 25 ) of it's specification.

-- In this test we limited the range of n to 25, 100 seems to be too slow, 50 also.
-- This is because the subsequences grow exponentially, so the time to calculate too.

-- This exercise took us 3 hours
