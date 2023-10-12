import Data.List

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos (x:[]) = [x]
trClos ((x,y):xs) = [(x,y)] ++ closeTillEnd [(x,y)] xs ++ trClos xs

closeTillEnd :: Ord a => Rel a -> Rel a -> Rel a
closeTillEnd r [] = []
closeTillEnd r ((x,y):xs) = do
    let res = (r @@ [(x,y)])
    if (length res) /= 0
    then res ++ closeTillEnd res xs
    else []