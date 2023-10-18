module Exercise1 where
import Data.List
import Test.QuickCheck
import System.Random
import SetOrd
import Control.Monad

coolSample :: IO [Set Int]
coolSample = do
  n <- randomRIO (2, 25)
  replicateM 100 $ do
    content <- fmap list2set $ replicateM n $ randomRIO (-100, 100)
    return content

coolCheck :: IO [Set Int] -> IO ()
coolCheck io = do
  sets <- io
  checkSets sets

checkSets :: [Set Int] -> IO ()
checkSets [] = putStrLn "+++ OK, passed 100 tests."
checkSets (x:xs) = do
  if prop_uniqueElements x
    then checkSets xs
    else putStrLn "Error: a set has non-unique elements!"

genSet :: Gen (Set Int)
genSet = do
  n <- choose (2, 25)
  content <- vectorOf n arbitrary
  return (list2set content)

prop_uniqueElements :: Set Int -> Bool
prop_uniqueElements s = uniqueElements (set2list s)

set2list :: Ord a => Set a -> [a]
set2list (Set xs) = xs

uniqueElements :: Eq a => [a] -> Bool
uniqueElements [] = True
uniqueElements (x:xs) = x `notElem` xs && uniqueElements xs

main :: IO ()
main = do
    quickCheck (forAll genSet prop_uniqueElements)
    sample <- coolSample
    -- print sample
    coolCheck coolSample

