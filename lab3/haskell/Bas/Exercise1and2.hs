import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable

-- Replace nth element in list
replace :: [a] -> (Int, a) -> [a]
replace xs (i, e) = before ++ [e] ++ after
  where
    (before, _:after) = splitAt i xs

-- Mutate a list by changing one element
changeElement :: [Integer] -> Gen [Integer]
changeElement xs = do
  num <- arbitrary :: Gen Integer
  index <- choose (0, length xs - 1)
  return (replace xs (index, num))

-- Return an empty list
emptyList :: [a] -> Gen [b]
emptyList _ = return []

-- Return an integer instead of list
notAList :: [Int] -> Gen Int
notAList xs = do
    num <- arbitrary :: Gen Int
    return num

-- Change a list of ints to a list of chars
charList :: [Int] -> Gen [Char]
charList xs = do
    chars <- arbitrary :: Gen [Char]
    return chars

countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Integer
countSurvivors n [] f = 0
countSurvivors n xs f =
    case testAllProps xs f of
        Just True -> (countSurvivors (n - 1) xs f) + 1
        Just False -> countSurvivors (n - 1) xs f



testAllProps :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Maybe Bool
testAllProps [] _ = Just True
testAllProps (property:xs) f = do
    let input = 1 -- choose(1,100)
    let mutation = addElements -- elements[addElements, removeElements, anyList]
    let result = mutate mutation property f input
    result_one <- generate result
    case result_one of
        Just True -> testAllProps xs f
        Just False -> Just False
        Nothing -> testAllProps xs f
    

main :: IO ()
main = do
  -- Generate and print 5 samples
    generatedSample <- generate (charList [1,2,3,4])

    print generatedSample