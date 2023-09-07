import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


main :: IO ()
main = do
    -- check both statements. Only natural numbers are taken as input.
    quickCheck (forAll genNatNum prop_Statement1)
    quickCheck (forAll genNatNum prop_Statement2)

-- Represents the lefthandside of the first statement in the excercise.
-- Only accepts natural numbers as input.
statement1LHS :: Int -> Int
statement1LHS 0 = 0
statement1LHS n | n < 0 = error "Input has to be a natural number"
                | otherwise = n^2 + statement1LHS (n - 1)

-- Represents the righthandside of the first statement in the excercise
-- Only accepts natural numbers as input.
statement1RHS :: Int -> Int
statement1RHS n | n < 0 = error "Input has to be a natural number"
                | otherwise = n * (n + 1) * (2 * n + 1) `div` 6

-- Checks whether LHS1 produces the same output as RHS1
prop_Statement1 n =
    statement1LHS n == statement1RHS n

-- Represents the lefthandside of the second statement in the excercise
-- Only accepts natural numbers as input.
statement2LHS :: Int -> Int
statement2LHS 0 = 0
statement2LHS n | n < 0 = error "Input has to be a natural number"
                | otherwise = n^3 + statement2LHS (n - 1)

-- Represents the righthandside of the second statement of the excercise
-- Only accepts natural numbers as input.
statement2RHS :: Int -> Int
statement2RHS n | n < 0 = error "Input has to be a natural number"
                | otherwise = (n * (n + 1) `div` 2)^2

-- Checks whether LHS2 produces the same output as RHS2
prop_Statement2 n =
    statement2LHS n == statement2RHS n

-- Generates a natural number (it's debatable, but for this excercise 0 is considered
-- to be a natural number as well)
genNatNum :: Gen Int
genNatNum = arbitrary `suchThat` (>= 0)