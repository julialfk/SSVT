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

-- Adds elements to the beginning of an output list
addElementsToBegin :: [Integer] -> Gen [Integer]
addElements xs = do
  nums <- arbitrary :: Gen [Integer]
  return $ nums ++ xs

-- Adds elements to the end of an output list
addElementsToEnd :: [Integer] -> Gen [Integer]
addElements xs = do
  nums <- arbitrary :: Gen [Integer]
  return $ xs ++ nums

-- Time spent: 1 hour
-- Some other possible mutations:
-- Reverse list
-- Randomize list