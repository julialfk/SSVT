import Data.List
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

getShape :: Int -> Int -> Int -> Shape
getShape x y z = 
    case (x + y) > z of
        True -> getTriangle x y z
        False -> do
            NoTriangle

getTriangle :: Int -> Int -> Int -> Shape
getTriangle x y z = 
    do
        if (x == y) && (y == z) then Equilateral else
            do
                let [a,b,c] = sort[x,y,z]
                if (((a^2) + (b^2)) == (c^2)) then Rectangular else
                    if ((x == y) || (y == z) || (x == z)) then Isosceles else
                        Other



    

