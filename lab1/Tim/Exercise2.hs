import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture1

subsequences :: [a] -> [[a]]

subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

prop_length :: [a] -> Property
prop_length l = length (subsequences l) == ( 2^ length(l))

alphaChar :: Gen Char
alphaChar = elements ([0..9])

-- Generates a list of lower and upper case characters
alphaString :: Gen String
alphaString = listOf alphaChar