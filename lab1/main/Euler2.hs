---- Problem 2: Even Fibonacci Numbers ----

getFib :: Int -> Int -> [Int]
getFib _ 100 = []
getFib x y = do 
    if x < 4000000 then [x] ++ getFib y z else []
    where 
        z = (x + y)

getSumEvenFib :: Int
getSumEvenFib = sum [x | x <- getFib 1 2, even x]

-- We could test this using a 2^n and n^2 functions, the fibonacci sequence falls
-- within these two functions after around n = 12. N^2 would be the lower bound and
-- 2^n would be the upper bound. If it falls in this boundry you have already more certainty.