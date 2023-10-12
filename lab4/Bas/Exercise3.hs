type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = do
    if elem (y,x) xs 
    then
        [(x,y)] ++ symClos xs
    else 
        [(x,y), (y,x)] ++ symClos xs

-- Time spent 5 minutes