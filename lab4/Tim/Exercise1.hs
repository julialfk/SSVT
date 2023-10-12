module Exercise1 where
import Data.List
import Test.QuickCheck
import SetOrd

genSet :: Gen (Set Int)
genSet = do
  n <- choose (2, 25)
  content <- vectorOf n arbitrary
  return (list2set content)

prop_uniqueElements :: Set Int -> Property
prop_uniqueElements s = property (uniqueElements (set2list s))

set2list :: Ord a => Set a -> [a]
set2list (Set xs) = xs

uniqueElements :: Eq a => [a] -> Bool
uniqueElements [] = True
uniqueElements (x:xs) = x `notElem` xs && uniqueElements xs

main :: IO ()
main = quickCheck (forAll genSet prop_uniqueElements)

