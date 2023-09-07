triangle :: Integer -> Integer -> Integer -> Shape

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle a b c
    | not valid = NoTriangle
    | a == b && b == c = Equilateral
    | a == b || b == c || c == a = Isosceles
    | a^2 + b^2 == c^2 || b^2 + c^2 == a^2 || c^2 + a^2 == b^2 = Rectangular
    | otherwise = Other
  where
    valid = a + b > c && b + c > a && c + a > b


-- just fill in some example shapes, and see that it works