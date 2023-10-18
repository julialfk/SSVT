module Exercise6 where
import Data.List
import Test.QuickCheck
import SetOrd

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Recursively loop thorugh all elements of the set, adding the element itself
-- and then add all its transitive relations in the rest of the list next to the
-- element using the closeTillEnd function.
trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos (x:[]) = [x]
trClos ((x,y):xs) = sort $ nub $ [(x,y)] ++ closeTillEnd [(x,y)] xs ++ trClos xs

-- Loop through all elements in the list, using the @@ function check if there
-- is a transitive relation from the given r set to the current element in the list.
-- If so, add it to the list.
closeTillEnd :: Ord a => Rel a -> Rel a -> Rel a
closeTillEnd r [] = []
closeTillEnd r ((x,y):xs) = do
    let res = (r @@ [(x,y)])
    if (length res) /= 0
    then res ++ closeTillEnd res xs
    else []


-- Properties for symClos:
-- If x,y in set, y,x should be there too
-- Does it have duplicates
-- Is it ordered

-- Properties for trClos:
-- 
-- Does it have duplicates
-- Is it ordered
-- If xRy and yRz then xRz
--

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
  return $ sort $ nub $ l1 ++ l2

genTrClosureRelation :: Gen (Rel a)
genTrClosureRelation = do
  n <- choose (1,10)
  l1 <- vectorOf n genPair
  let trSet = [trClos l | l <- l1]
  return trSet

genPair :: Gen (a, a)
genPair = do
  x <- choose (1, 9)
  y <- choose (1, 9)
  return (x, y)

-- Check if the symmetric closure of each set in the 
-- list is present in the list. 
propSymExists :: Eq a => Rel a -> Bool
propSymExists rel = all (\(x, y) -> any (\(a, b) -> (a, b) == (y, x)) rel) rel

-- Check if all elements in the list are unique
propIsUnique :: Eq a => Rel a -> Bool
propIsUnique [] = True
propIsUnique (x:xs) =
    if elem x xs
        then False
        else propIsUnique xs

-- Check if the list is ordered
propIsOrdered :: Ord a => Rel a -> Bool
propIsOrdered ls =
    if sort ls == ls 
        then True
        else False

propElementInOtherRel :: Ord a => Rel a -> Bool
propElementInOtherRel [] = True
propElementInOtherRel ((x,y):xs) =
  if ((loopTillEnd x xs) && (loopTillEnd y xs)) then
    True
  else False

loopTillEnd :: Eq a => a -> Rel a -> Bool
loopTillEnd y [] = False
loopTillEnd y ((r, u):xs) = 
  if (y == r || y == u) then True
  else loopTillEnd y xs


main :: IO ()
main = do
    quickCheck (forAll genSymClosureRelation propIsOrdered)
    quickCheck (forAll genSymClosureRelation propIsUnique)
    quickCheck (forAll genSymClosureRelation propSymExists)

    quickCheck (forAll genTrClosureRelation propIsOrdered)
    quickCheck (forAll genTrClosureRelation propIsUnique)
    quickCheck (forAll genTrClosureRelation propElementInOtherRel)


-- Indication of time spent: 1.5 hours