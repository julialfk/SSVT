import Test.QuickCheck

-- Properties for symClos:
-- If x,y in set, y,x should be there too
type Rel a = [(a,a)]

propSymExists :: Rel a -> Bool
propSymExists [] = True
propSymExists ((x,y):xs) = do
    if elem (y,x) xs
        then propSymExists xs
        else False


