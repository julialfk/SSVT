import Data.List
import SetOrd

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = sort $ nub $ (x,y):(y,x):symClos xs

-- Time spent 10 minutes