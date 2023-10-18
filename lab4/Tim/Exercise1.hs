module Exercise1 where
import Data.List
import Test.QuickCheck
import System.Random
import SetOrd
import Control.Monad

-- Creates a sample list with use of replication functions and randomRIO
coolSample :: IO [Set Int]
coolSample = do
  n <- randomRIO (2, 25)
  replicateM 100 $ do
    content <- fmap list2set $ replicateM n $ randomRIO (-100, 100)
    return content

-- Executes our own check
coolCheck :: IO [Set Int] -> IO ()
coolCheck io = do
  sets <- io
  checkSets sets

-- Checks if sets consists of unique elements
checkSets :: [Set Int] -> IO ()
checkSets [] = putStrLn "+++ OK, passed 100 tests. (CoolCheck)"
checkSets (x:xs) = do
  if prop_uniqueElements x
    then checkSets xs
    else putStrLn "Error: a set has non-unique elements!"

-- Generates set of integers
genSet :: Gen (Set Int)
genSet = do
  n <- choose (2, 25)
  content <- vectorOf n arbitrary
  return (list2set content)

-- Property to check if elements are unique
prop_uniqueElements :: Set Int -> Bool
prop_uniqueElements s = uniqueElements (set2list s)

-- Converts set to list
set2list :: Ord a => Set a -> [a]
set2list (Set xs) = xs

-- Recursive function to check if element is not multiple times in list
uniqueElements :: Eq a => [a] -> Bool
uniqueElements [] = True
uniqueElements (x:xs) = x `notElem` xs && uniqueElements xs

-- Main function, runs CoolCheck (our check) and QuickCheck
main :: IO ()
main = do
    quickCheck (forAll genSet prop_uniqueElements)
    sample <- coolSample
    print sample
    coolCheck coolSample


-- Time spent: 150 min

