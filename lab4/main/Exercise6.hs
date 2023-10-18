module Exercise6 where
import Data.List
import Test.QuickCheck
import SetOrd
import qualified Control.Arrow as closures

-- Properties for symClos:
-- If x,y in set, y,x should be there too
-- Does it have duplicates
-- Is it ordered

-- Properties for trClos:
--
-- Does it have duplicates
-- Is it ordered
-- Check if xRy, and yRz then xRz also exists

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Recursively loop thorugh all elements of the set, adding the element itself
-- and then add all its transitive relations in the rest of the list next to the
-- element using the closeTillEnd function.
trClos :: [(Int, Int)] -> [(Int, Int)]
trClos [] = []
trClos (x:[]) = [x]
trClos ((x,y):xs) = sort $ nub $ [(x,y)] ++ closeTillEnd [(x,y)] xs ++ trClos xs

-- Loop through all elements in the list, using the @@ function check if there
-- is a transitive relation from the given r set to the current element in the list.
-- If so, add it to the list.
closeTillEnd :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
closeTillEnd r [] = []
closeTillEnd r ((x,y):xs) = do
    let res = (r @@ [(x,y)])
    if (length res) /= 0
    then res ++ closeTillEnd res xs
    else []

type Rel a = [(a,a)]

-- Generates relation
genRelation :: Gen [(Int, Int)]
genRelation = do
  n <- choose (0, 10)
  l <- vectorOf n genPair
  return l

-- Generates relations that symmertic closures
genSymClosureRelation :: Gen [(Int, Int)]
genSymClosureRelation = do
  n <- choose (0, 5)
  l1 <- vectorOf n genPair
  let l2 = [(y, x) | (x, y) <- l1]
  return $ sort $ nub $ l1 ++ l2

-- Generates relations that are transitive closures
genTrClosureRelation :: Gen [(Int, Int)]
genTrClosureRelation = do
  n <- choose (1,10)
  l1 <- vectorOf n genPair
  return (trClos l1)

-- Generates a pair, that can be used for generating relations
genPair :: Gen (Int, Int)
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

-- Main function to run the properties
main :: IO ()
main = do
    quickCheck (forAll genSymClosureRelation propIsOrdered)
    quickCheck (forAll genSymClosureRelation propIsUnique)
    quickCheck (forAll genSymClosureRelation propSymExists)

    quickCheck (forAll genTrClosureRelation propIsOrdered)
    quickCheck (forAll genTrClosureRelation propIsUnique)

-- The properties that are tested all comply with the rules of symmetric closures and transitive closures.
-- We checked that the relations are ordered and unique, and for the symmetric ones, we also checked for symmetry.


-- Indication of time spent: 1.5 hours
