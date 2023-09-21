module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import Control.Exception


-- Indication of time spent: 5 hours.

main :: IO ()
main = do
    -- Test whether the factorial of n is larger or equal to n. All tests pass.
    quickCheck (forAll genNonNeg prop_size) 
    
    -- Test whether the factorial of n is even (when n >= 2), or 1 (when n = 0 
    -- r 1), or an error (when n < 0). All tests pass.
    quickCheck prop_Even

    -- Test whether the factorial of n + 1 is equal to the factorial of n multiplied by (n + 1).
    -- All tests pass.
    quickCheck (forAll genNonNeg prop_Next)

-- This function accepts all integers, but will throw an error when n < 0,
-- because the factorial of a negative number does not exist.
factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n < 0 = error "n cannot be negative"
            | otherwise = n * factorial (n - 1)

-- The factorial of a number should be equal to or greater than that number.
-- The nonNegative number generator is used; the error message for negative numbers is tested elsewhere (see prop_even)
prop_size :: Integer -> Property
prop_size n = property (factorial n >= n)

-- If n >= 2: factorial n should be an even number (because you always multiply by an even number at some point)
-- If n < 0: an error should be thrown (but the program does not terminate; the test keeps on running)
-- If n is 0 or 1: factorial n should be 1.
-- The idea behind this property is that the function factorial in principle does accept all integers; however for
-- some integers, an error is returned. Because this error is expected behaviour, it should also be tested for.
-- At first this test also used the genNonNeg generator, but we decided that this was insufficient because the function does actually
-- have expected behaviour for n < 0.
-- Testing for the error was a struggle because the programs usually terminate after an exception, therefore ending
-- and failing the quickCheck test. After quite some hours this issue was resolved.
-- In the current implementation, the property actually checks for the expected error message and continues testing.
-- We therefore think that this is a more complete test than testing for an even factorial with the input being a nonnegative number.
-- Implementation details inspired by: https://stackoverflow.com/questions/33451293/is-it-possible-to-check-cases-when-exception-is-thrown-with-quickcheck?rq=3
prop_Even :: Integer -> Property
prop_Even n = ioProperty $ do
    result <- try . evaluate $ factorial n
    return $
        case result of
            Left (ErrorCall msg) -> n < 0 && msg == "n cannot be negative"
            Right x -> checkFactorial n x

-- The factorial of the next integer (n + 1) should be equal to the factorial of n, multiplied by (n + 1)
-- Because the error is properly checked in the prop_Even, we use the nonnegative number generator for this property.
prop_Next :: Integer -> Property
prop_Next n = property (factorial (n + 1) == (n + 1) * factorial n)

--  Helper function for prop_Even
checkFactorial :: Integer -> Integer -> Bool
checkFactorial n x  | n < 2 = x == 1
                    | otherwise = even x

-- Generates a nonnegative number
genNonNeg :: Gen Integer
genNonNeg = arbitrary `suchThat` (>= 0)
