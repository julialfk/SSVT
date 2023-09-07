import Data.Char
import Test.QuickCheck

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (x:xs)
    | isAsciiLower x = lowerRotation x : rot13 xs
    | isAsciiUpper x = upperRotation x : rot13 xs
    | otherwise      = x : rot13 xs

-- Shifts a lower case character by 13 places
lowerRotation :: Char -> Char
lowerRotation c = chr (((ord c - ord 'a' + 13) `mod` 26) + ord 'a')

-- Shifts an upper case character by 13 places
upperRotation :: Char -> Char
upperRotation c = chr (((ord c - ord 'A' + 13) `mod` 26) + ord 'A')

-- This property checks whether reversing a rotated string twice gets you back
-- to the original string
propROT13Double :: [Char] -> Bool
propROT13Double s = rot13 (rot13 s) == s

-- This property checks whether reversing once gets you a different string than the original one.
-- Empty strings are discarded because those will of course stay the same after reversal.
propROT13Single :: [Char] -> Property
propROT13Single s =
            not (null s) ==> s /= rot13 s

-- Generates an alphabetic lower or upper case character
alphaChar :: Gen Char
alphaChar = elements (['a'..'z'] ++ ['A'..'Z'])

-- Generates a list of lower and upper case characters
alphaString :: Gen String
alphaString = listOf alphaChar

main :: IO ()
main = do
    quickCheck propROT13Double
    quickCheck (forAll alphaString propROT13Single)
