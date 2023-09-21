import System.Random
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck (forAll genPosNum prop_subsequences)

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

prop_subsequences :: Integer -> Bool
prop_subsequences n =  length (subsequences [1..n]) == 2^n

genPosNum :: Gen Integer
genPosNum = arbitrary `suchThat` (>= 0)

-- Question 1 - Is the property hard to test? If you find that it is, can you given a reason why?
    -- The property is hard to test because it takes a lot of time to compute.
    -- We think this is because of the nature of the subsequences function:
    -- for higher n-values, it has to go through a lot of recursive steps to
    -- create the list of subsequences.

-- Question 2 - Give your thoughts on the following issue:
-- When you perform the test for exercise 4, what are you testing actually?
-- Are you checking a mathematical fact? Or are you testing whether
-- subsequences satisfies a part of its specification? Or are you testing
-- something else still?
    -- ..? TO DO