module Exercise3 where

import Data.List
import SetOrd

-- Time spent: 20 minutes

type Rel a = [(a,a)]

-- Add the symmetric closure of a set next to it in the list. Do this for all sets in the list.
-- Then remove the duplicates. After this sort it.
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = sort $ nub $ (x,y):(y,x):symClos xs
