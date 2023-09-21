import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture1

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

genInts :: Gen Integer
genInts = arbitrary `suchThat` (>= 0)

newList :: Gen [Integer]
newList = listOf genInts

checkParity :: Integer -> Bool
checkParity n | n == 1 || n == 0 = factorial n == 1
              | otherwise = even (factorial n)

checkNext :: Integer -> Bool
checkNext n = factorial (n + 1) == (n + 1) * factorial n

main :: IO()
main = do
    quickCheck $ forAll newList $ all checkParity
    quickCheck $ forAll newList $ all checkNext

-- time spent: 1 hr
