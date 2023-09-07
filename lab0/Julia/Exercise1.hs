import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Using recursion to sum up the sequences seemed the easiest.
sumCubes :: Int -> Int
sumCubes 0 = 0
sumCubes n = n ^ 3 + sumSquares (n - 1)

formulaCubes :: Int -> Int
formulaCubes n = ((n * (n + 1)) `div` 2) ^ 2

-- Added a conditional to only allow natural numbers.
checkCubes :: Int -> Property
checkCubes n = n >= 0 ==> sumCubes n == formulaCubes n

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n ^ 2 + sumSquares (n - 1)

formulaSquares :: Int -> Int
formulaSquares n = ((n * (n + 1)) * (2 * n + 1)) `div` 6

checkSquares :: Int -> Property
checkSquares n = n >= 0 ==> sumSquares n == formulaSquares n

-- To check the statements, run 'quickCheck checkSquares/checkCubes'.

-- time spent: 1 hr
