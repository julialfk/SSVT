import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Data.Maybe

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
countSurvivors 0 xs f = 0
countSurvivors n xs f = do
    let properties = xs
    let input = 10
    let mutation = addElements -- [addElements, removeElements, anyList]
    let result = generate (mutate' addElements xs f 10)
    case (fmap (elem False) result) of
        True -> countSurvivors (n - 1) xs f
        False -> (countSurvivors (n - 1) xs f) + 1


-- testAllProps :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Bool
-- testAllProps [] _ = True
-- testAllProps (property:xs) f = do
--     let input = 1 -- choose(1,100)
--     let mutation = addElements -- elements[addElements, removeElements, anyList]
--     let result = generate (mutate' mutation [property] f input)
--     case result of 
--         [True] -> testAllProps xs f
--         [False] -> False
--         [] -> testAllProps xs f

-- Define a property to test using mutate
-- myProperty :: Integer -> Property
-- myProperty input =
--   forAll (mutate arbitraryMutator arbitraryProperty arbitraryFut input) $ \result ->
--     case result of
--       Just boolResult -> classify boolResult "Success" $ boolResult === True
--       Nothing -> classify True "Mutation failed" False




main :: IO ()
main = do
  -- Generate and print 5 samples
    generatedSample <- generate (charList [1,2,3,4])

    print generatedSample