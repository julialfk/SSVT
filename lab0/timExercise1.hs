sequence1 :: [Int]
sequence1 = map (^2) [1..]

equation1 :: Int -> Int
equation1 n = (n*(n+1)*(2*n+1)) `div` 6

answer1 :: Int -> Int
answer1 n = sum (take n sequence1)

-- next equation

sequence2 :: [Int]
sequence2 = map (^3) [1..]

equation2 :: Int -> Int
equation2 n = ((n*(n+1)) `div` 2) ^ 2

answer2 :: Int -> Int
answer2 n = sum (take n sequence2)

--30 minutes