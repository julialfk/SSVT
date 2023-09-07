import Test.QuickCheck

sequence1 :: [Int]
sequence1 = map (^2) [1..]

equation1 :: Int -> Int
equation1 n = (n*(n+1)*(2*n+1)) `div` 6

answer1 :: Int -> Int
answer1 n = sum (take n sequence1)

sequence2 :: [Int]
sequence2 = map (^3) [1..]

equation2 :: Int -> Int
equation2 n = ((n*(n+1)) `div` 2) ^ 2

answer2 :: Int -> Int
answer2 n = sum (take n sequence2)

prop_Equation1 :: Int -> Property
prop_Equation1 n = n >= 0 ==> answer1 n == equation1 n

prop_Equation2 :: Int -> Property
prop_Equation2 n = n >= 0 ==> answer2 n == equation2 n

main :: IO ()
main = do
  quickCheck prop_Equation1
  quickCheck prop_Equation2

--60 minutes