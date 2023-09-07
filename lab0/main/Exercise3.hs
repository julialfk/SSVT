import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)


-- The program is tested by using quickCheck. Each different possible shape has its own test.
-- See comments below for the specific implementation.
-- Time spent: 1.5 hours. Most time was spent on quickcheck. The function itself took a lot less time.

main :: IO ()
main = do
    -- quickCheck (forAll positiveTriplets prop_Triangle)
    quickCheck (forAll positiveTriplets prop_NoTriangle)
    quickCheck (forAll equalTriplets prop_Equilateral)
    quickCheck (forAll isoscelesTriplets prop_Isosceles)
    quickCheck (forAll rectangularTriplets prop_Rectangular)
    quickCheck (forAll otherTriangleTriplets prop_Other)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | a + b < c || a + c < b || b + c < a = NoTriangle
                | a == b && b == c = Equilateral
                | a == b || b == c || a == c = Isosceles
                | a^2 + b^2 == c^2 || b^2 + c^2 == a^2 || a^2 + c^2 == b^2 = Rectangular
                | otherwise = Other

-- for all triangles: the sum of the lengths of any two sides must be greater than or equal to the length of the remaining side. Taken from wikipedia
-- for this test, the filtering is done inside the test.
prop_NoTriangle :: (Integer, Integer, Integer) -> Property
prop_NoTriangle (a, b, c) =
            a + b < c || a + c < b || b + c < a ==> triangle a b c == NoTriangle

-- filtering for valid input is done at the number generator "equalTriplets"
prop_Equilateral :: (Integer, Integer, Integer) -> Bool
prop_Equilateral (a, b, c) =
            triangle a b c == Equilateral

-- filtering for valid input is done at the number generator "isoscelesTriplets"
prop_Isosceles :: (Integer, Integer, Integer) -> Bool
prop_Isosceles (a, b, c) =
            triangle a b c == Isosceles

-- filtering for valid input is done at the number generator "rectangularTriplets"
prop_Rectangular :: (Integer, Integer, Integer) -> Bool   
prop_Rectangular (a, b, c) =
            triangle a b c == Rectangular

-- filtering for valid input is done at the number generator "otherTriangleTriplets"
prop_Other :: (Integer, Integer, Integer) -> Bool
prop_Other (a, b, c) =
            triangle a b c == Other

-- This was the first property built. Later it was decided to split this one up
-- into different test cases that each look into a different shape/property.
prop_Triangle :: Integer -> Integer -> Integer -> Bool
prop_Triangle a b c =
    case triangle a b c of
        NoTriangle -> isInvalidTriangle a b c
        Equilateral -> isEquilateral a b c
        Isosceles -> isIsosceles a b c
        Rectangular -> isRectangular a b c
        Other -> isOther a b c
    where
        isInvalidTriangle x y z = x + y < z || x + z < y || y + z < x
        isEquilateral x y z = x == y && y == z
        isIsosceles x y z = x == y || y == z || x == z
        isRectangular x y z = x^2 + y^2 == z^2 || y^2 + z^2 == x^2 || x^2 + z^2 == y^2
        isOther _ _ _ = True

-- Generates a triplet of positive integers
positiveTriplets :: Gen (Integer, Integer, Integer)
positiveTriplets = do
    a <- arbitrary `suchThat` (> 0)
    b <- arbitrary `suchThat` (> 0)
    c <- arbitrary `suchThat` (> 0)
    return (a, b, c)

--  Generates a triplet three equal positive integers
equalTriplets :: Gen (Integer, Integer, Integer)
equalTriplets = do
    a <- arbitrary `suchThat` (> 0)
    return (a, a, a)

-- Generates a triplet of three positive integers of which two are the same value
isoscelesTriplets :: Gen (Integer, Integer, Integer)
isoscelesTriplets = do
    a <- arbitrary `suchThat` (> 0)
    b <- arbitrary `suchThat` (> 0)
    c <- arbitrary `suchThat` (> 0)
    let isValidTriangle = a + b >= c && a + c >= b && b + c >= a
    if isValidTriangle && ((a == b && a /= c) || (b == c && b /= a) || (a == c && a /= b))
        then return (a, b, c)
        else isoscelesTriplets

-- Generates a triplet of three positive integers that together have the pythagorian
-- property of a^2 + b^2 = c^2
rectangularTriplets :: Gen (Integer, Integer, Integer)
rectangularTriplets = do
    a <- arbitrary `suchThat` (> 0)
    b <- arbitrary `suchThat` (> 0)
    c <- arbitrary `suchThat` (> 0)
    let isValidTriangle = a + b >= c && a + c >= b && b + c >= a
    if isValidTriangle && (a^2 + b^2 == c^2 || b^2 + c^2 == a^2 || a^2 + c^2 == b^2)
        then return (a, b, c)
        else rectangularTriplets

-- Generates a triplet of three positive integers that together form a triangle
-- that is not an equilateral, isosceles or a rectangular triangle 
otherTriangleTriplets :: Gen (Integer, Integer, Integer)
otherTriangleTriplets = do
    a <- arbitrary `suchThat` (> 0)
    b <- arbitrary `suchThat` (> 0)
    c <- arbitrary `suchThat` (> 0)
    let isValidTriangle = a + b >= c && a + c >= b && b + c >= a
    if isValidTriangle && a /= b && b /= c && a /= c && a^2 + b^2 /= c^2 && b^2 + c^2 /= a^2 && a^2 + c^2 /= b^2
        then return (a, b, c)
        else otherTriangleTriplets
