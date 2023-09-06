proof1 :: [Integer]
proof1 = map (^2) [1..]

equation1 :: Integer -> Integer
equation1 n = (n*(n+1)*(2*n+1)) `div` 6

answer1 :: Integer -> Integer
answer1 n = sum (take n proof1)

-- next equation

proof2 :: [Integer]
proof2 = map (^3) [1..]

equation2 :: Integer -> Integer
equation2 n = ((n*(n+1)) `div` 2) ^ 2

answer2 :: Integer -> Integer
answer2 n = sum (take n proof2)

--30 minutes