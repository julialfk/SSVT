import System.Random
import Test.QuickCheck

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forAll xs (\ x -> p x --> q x) -- variable '-->' not in scope..??
weaker xs p q = stronger xs q p


prop1 :: Int -> Bool
prop1 x = stronger [-10..10] (\ x -> even x && x > 3) even

prop2 :: Int -> Bool
prop2 x =  stronger [-10..10] (\ x -> even x || x > 3) even

prop3 :: Int -> Bool
prop3 x = stronger [-10..10] (\ x -> (even x && x > 3) || even x) even

prop4 :: Int -> Bool
prop4 x =  stronger [-10..10] even (\ x -> (even x && x > 3) || even x)

-- TO DO: descending strength list of said implementation (figure out how to print
        -- them, and provide your answer in a comment too), an indication of time spent.
