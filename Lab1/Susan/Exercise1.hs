import Data.List
import System.Random
import Test.QuickCheck
import Control.Exception

-- 4 hours spent

main :: IO ()
main = do
    quickCheck (forAll genPosNum prop_1) 
    quickCheck (forAll genPosNum prop_2) -- Old check that doesn't actually check for the exception handling because input constraints
    quickCheck prop_3  -- check that does check for the exception handling; more complete than prop_2 because no input constraints

factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n < 0 = error "n cannot be negative"
            | otherwise = n * factorial (n - 1)

prop_1 :: Integer -> Bool
prop_1 n = factorial n >= n

prop_2 :: Integer -> Bool
prop_2 n    | n < 0 = factorial n == error "n cannot be negative"
            | n < 2 = factorial n == 1
            | otherwise = even $ factorial n

-- Implementation details inspired by: https://stackoverflow.com/questions/33451293/is-it-possible-to-check-cases-when-exception-is-thrown-with-quickcheck?rq=3
prop_3 :: Integer -> Property
prop_3 n = ioProperty $ do
    result <- try . evaluate $ factorial n
    return $
        case result of
            Left (ErrorCall msg) -> n < 0 && msg == "n cannot be negative"
            Right x -> checkFactorial n x

checkFactorial :: Integer -> Integer -> Bool
checkFactorial n x  | n < 2 = x == 1
                    | otherwise = even x

genPosNum :: Gen Integer
genPosNum = arbitrary `suchThat` (>= 0)
