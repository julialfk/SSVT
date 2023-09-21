getMultiples :: [Int]
getMultiples = [x | x <- [1..1000], (x `mod` 5 == 0) || (x `mod` 3 == 0)]

foldList :: Int
foldList = foldr (+) 0 getMultiples

getFib :: Int -> Int -> [Int]
getFib _ 100 = []
getFib x y = do 
    if x < 4000000 then [x] ++ getFib y z else []
    where 
        z = (x + y)

getSumEvenFib :: Int
getSumEvenFib = sum [x | x <- getFib 1 2, even x]