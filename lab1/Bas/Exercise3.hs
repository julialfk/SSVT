module Exercise3 where
import Test.QuickCheck
import Data.List
import System.Random
import Test.QuickCheck
import Lecture2


-- Get a list of 100 int numbers between -10 and +10.
positiveIntListGenerator :: Gen [Int]
positiveIntListGenerator = vectorOf 100 (arbitrary `suchThat` (\x -> x > -10 && x < 10))

-- Generate the list mentioned above
randomList :: IO [Int]
randomList = generate positiveIntListGenerator

-- Check if an int is even and bigger than three. (Prop 1)
biggerThanAndEven :: Int -> Bool
biggerThanAndEven x = ((even x) && (x>3))

-- Check if an int is even or bigger than three. (Prop 2)
biggerThanOrEven :: Int -> Bool
biggerThanOrEven x = ((even x) || (x>3))

-- Check if an int is even and bigger than three.. or even. (Prop 3)
biggerThanAndEvenOrEven :: Int -> Bool
biggerThanAndEvenOrEven x = (((even x) && (x > 3)) || even x)

-- The fourth property has been left out as it was the exact same as property 3 when
-- translated into code.

-- Calculate how many times a property is stronger than the others. This will generate a list
-- that shows the points in order of the properties that were added.
getPos :: [Int] -> (Int -> Bool) -> [(Int -> Bool)] -> Int
getPos xs p [] = 0
getPos xs p (q:qs) = 
    case stronger xs p q of
        True -> 1 + getPos xs p qs
        False -> 0 + getPos xs p qs

-- The order of the amount of wins for each property is the same as the order of the properties
-- in the list. These two need to be linked to print them in the correct order.
printOrder :: [String] -> [String] -> [Int] -> [Int] -> Int -> IO ()
printOrder xs oxs [] os pos = printOrder oxs oxs os os pos
printOrder xs oxs ls os (-1) = return ()
printOrder (x:xs) oxs (l:ls) os pos = 
    case l == pos of
        True -> do
            putStrLn(show x)
            printOrder xs oxs ls os (pos - 1)
        False -> printOrder xs oxs ls os pos

-- Run all functions in the proper order to get the results.
main :: IO ()
main = do
    randomIntList <- randomList
    let functionList = [biggerThanAndEven, biggerThanOrEven, biggerThanAndEvenOrEven]
    let firstPropPos = getPos randomIntList (functionList !! 0) [(functionList !! 1), (functionList !! 2)]
    let secondPropPos = getPos randomIntList (functionList !! 1) [(functionList !! 0), (functionList !! 2)]
    let thirdPropPos = getPos randomIntList (functionList !! 2) [(functionList !! 0), (functionList !! 1)]
    let posList = [firstPropPos, secondPropPos, thirdPropPos]
    let functions = ["((even x) && (x>3))", "(even x) || (x>3))", "(((even x) && (x > 3)) || even x)"]
    printOrder functions functions posList posList ((length functionList) - 1) 

-- Order of properties from strongest to weakest:
-- "((even x) && (x>3))"
-- "(((even x) && (x > 3)) || even x)"
-- "(even x) || (x>3))"
-- 
-- Time spent: 2,5 hours