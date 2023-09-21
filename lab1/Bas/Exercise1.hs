import Test.QuickCheck

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = factorial (x - 1) * x

positiveIntListGenerator :: Gen [Int]
positiveIntListGenerator = vectorOf 100 (arbitrary `suchThat` (> 0))
-- positiveIntListGenerator = vectorOf 100 arbitrary

randomPositiveIntList :: IO [Int]
randomPositiveIntList = generate positiveIntListGenerator

propFactorialIsPositive :: Property
propFactorialIsPositive = all (>0) [factorial x | x <- randomPositiveIntList]