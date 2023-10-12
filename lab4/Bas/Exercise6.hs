module Exercise6 where
import Data.List
import Test.QuickCheck
import SetOrd

-- Properties for symClos:
-- If x,y in set, y,x should be there too
type Rel a = [(a,a)]

genRelation :: Gen [(Int, Int)]
genRelation = do
  n <- choose (0, 10)
  sequence [genPair | _ <- [1..n]]

genPair :: Gen (Int, Int)
genPair = do
  x <- choose (1, 9)
  y <- choose (1, 9)
  return (x, y)

propSymExists :: Rel a -> Bool
propSymExists [] = True
propSymExists ((x,y):xs) = do
    if elem (y,x) xs
        then propSymExists xs
        else False


