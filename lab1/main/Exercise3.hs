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

-- Check if an int is even and bigger than three. (Implementation of property 1)
biggerThanAndEven :: Int -> Bool
biggerThanAndEven x = ((even x) && (x>3))

-- Check if an int is even or bigger than three. (Implementation of property 2)
biggerThanOrEven :: Int -> Bool
biggerThanOrEven x = ((even x) || (x>3))

-- Check if an int is even and bigger than three.. or even. (Implementation of property 3)
biggerThanAndEvenOrEven :: Int -> Bool
biggerThanAndEvenOrEven x = (((even x) && (x > 3)) || even x)

-- The fourth property has been left out as it was the exact same as property 3 when
-- translated into code.

-- This function compares properties to each other, determining if the first given
-- property is stronger. Based on this the property gets points for each win, 
-- the output is the amount of times the property was stronger.
getPos :: [Int] -> (Int -> Bool) -> [(Int -> Bool)] -> Int
getPos xs p [] = 0
getPos xs p (q:qs) = 
    case stronger xs p q of
        True -> 1 + getPos xs p qs
        False -> 0 + getPos xs p qs

-- The function associated with the highest score from the previous function should be printed 
-- first. To do this, both list get saved and unchanged, we loop throught both list at the same
-- time until the best score is found. The best score is the length of the list, as this
-- means the property won all comparisons.
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
    -- Get the amount of wins for each property.
    let firstPropPos = getPos randomIntList (functionList !! 0) [(functionList !! 1), (functionList !! 2)]
    let secondPropPos = getPos randomIntList (functionList !! 1) [(functionList !! 0), (functionList !! 2)]
    let thirdPropPos = getPos randomIntList (functionList !! 2) [(functionList !! 0), (functionList !! 1)]
    -- Turn the wins into a list ordered by properties.
    let posList = [firstPropPos, secondPropPos, thirdPropPos]
    -- A list of the implementation of properties to print.
    let functions = ["((even x) && (x>3))", "(even x) || (x>3))", "(((even x) && (x > 3)) || even x)"]
    printOrder functions functions posList posList ((length functionList) - 1) 

-- Order of properties from strongest to weakest:
-- "((even x) && (x>3))"
-- "(((even x) && (x > 3)) || even x)"
-- "(even x) || (x>3))"
-- 
-- Time spent: 2,5 hours