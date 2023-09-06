proof1 :: [Int]
proof1 = map (^2) [1..]

equation1 :: Int -> Int
equation1 n = (n*(n+1)*(2*n+1)) `div` 6

answer1 :: Int -> Int
answer1 n = sum (take n proof1)

-- next equation

proof2 :: [Int]
proof2 = map (^3) [1..]

equation2 :: Int -> Int
equation2 n = ((n*(n+1)) `div` 2) ^ 2

answer2 :: Int -> Int
answer2 n = sum (take n proof2)

--30 minutes