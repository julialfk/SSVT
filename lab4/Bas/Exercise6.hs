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
  l <- vectorOf n genPair
  return l

genSymClosureRelation :: Gen [(Int, Int)]
genSymClosureRelation = do
  n <- choose (0, 5)
  l1 <- vectorOf n genPair
  let l2 = [(y, x) | (x, y) <- l1]
  return $ nub $ l1 ++ l2

genPair :: Gen (Int, Int)
genPair = do
  x <- choose (1, 9)
  y <- choose (1, 9)
  return (x, y)

-- propSymExists :: Rel a -> Bool
-- propSymExists [] = True
-- propSymExists ((x,y):xs) = do
--     if elem (y,x)
--         then propSymExists xs
--         else False


