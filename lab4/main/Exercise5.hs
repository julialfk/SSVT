import Data.List

type Rel a = [(a,a)]

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

-- Time spent: 60 minutes