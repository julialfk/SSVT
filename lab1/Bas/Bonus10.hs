---- Problem 1: Multiples of 3 and 5 ----

getMultiples :: Int
getMultiples = sum [x | x <- [1..1000], (x `mod` 5 == 0) || (x `mod` 3 == 0)]

-- You could test the sum with an upper boundry and lower boundry.
-- An estimate of a lower boundry could be 1000, because 1000 is the highest number that can be divided by 5.
-- A highest boundry would be that every number would be divisible. Which is just the sum of 0-1000 which is 500500.
-- If it falls in this boundry you have already more certainty.


---- Problem 2: Even Fibonacci Numbers ----

getFib :: Int -> Int -> [Int]
getFib x y = do 
    if x < 4000000 then [x] ++ getFib y z else []
    where 
        z = (x + y)

getSumEvenFib :: Int
getSumEvenFib = sum [x | x <- getFib 1 2, even x]

-- We could test this using a 2^n and n^2 functions, the fibonacci sequence falls
-- within these two functions after around n = 12. N^2 would be the lower bound and
-- 2^n would be the upper bound. If it falls in this boundry you have already more certainty.