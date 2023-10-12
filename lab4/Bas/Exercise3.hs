import Data.List

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = do
    if elem (y,x) xs 
    then 
        do 
            let newList = delete (y,x) xs
            if x < y 
            then [(x,y), (y,x)] ++ symClos newList
            else [(y,x), (x,y)] ++ symClos newList
    else 
        if x < y 
        then [(x,y), (y,x)] ++ symClos xs
        else [(y,x), (x,y)] ++ symClos xs

-- Time spent 10 minutes