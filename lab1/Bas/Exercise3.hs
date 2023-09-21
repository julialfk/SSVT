import Test.QuickCheck
import Data.List
import System.Random
import Test.QuickCheck
import Lecture2

positiveIntListGenerator :: Gen [Int]
positiveIntListGenerator = vectorOf 100 (arbitrary `suchThat` (\x -> x > -10 && x < 10))

randomList :: IO [Int]
randomList = generate positiveIntListGenerator

biggerThanAndEven :: Int -> Bool
biggerThanAndEven x = ((even x) && (x>3))

biggerThanOrEven :: Int -> Bool
biggerThanOrEven x = ((even x) || (x>3))

biggerThanAndEvenOrEven :: Int -> Bool
biggerThanAndEvenOrEven x = (((even x) && (x > 3)) || even x)

evenOrbiggerThanAndEven :: Int -> Bool
evenOrbiggerThanAndEven x = (((even x) && (x > 3)) || even x)

getPos :: [Int] -> (Int -> Bool) -> [(Int -> Bool)] -> Int
getPos xs p [] = 0
getPos xs p (q:qs) = 
    case stronger xs p q of
        True -> 1 + getPos xs p qs
        False -> 0 + getPos xs p qs

printOrder :: [String] -> [String] -> [Int] -> [Int] -> Int -> IO ()
printOrder xs oxs [] os pos = printOrder oxs oxs os os pos
printOrder xs oxs ls os (-1) = return ()
printOrder (x:xs) oxs (l:ls) os pos = 
    case l == pos of
        True -> do
            putStrLn(show x)
            printOrder xs oxs ls os (pos - 1)
        False -> printOrder xs oxs ls os pos

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
-- Time spent: 2,5 hours