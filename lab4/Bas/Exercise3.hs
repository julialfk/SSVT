type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = do
    if elem (y,x) xs 
    then [(x,y)] ++ symClos xs
    else 
        if x < y 
        then [(x,y), (y,x)] ++ symClos xs
        else [(y,x), (x,y)] ++ symClos xs

-- Time spent 10 minutes