import Data.Char
import Test.QuickCheck

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (x:xs)
    | isAsciiLower x = lowerRotation x : rot13 xs
    | isAsciiUpper x = upperRotation x : rot13 xs
    | otherwise      = x : rot13 xs


lowerRotation :: Char -> Char
lowerRotation c = chr (((ord c - ord 'a' + 13) `mod` 26) + ord 'a')

upperRotation :: Char -> Char
upperRotation c = chr (((ord c - ord 'A' + 13) `mod` 26) + ord 'A')

-- isAlphabetic :: [Char] -> Bool
-- isAlphabetic = all isAlpha

-- prop_rot13_twice :: [Char] -> Property
-- prop_rot13_twice s = not (null s) ==> isAlphabetic s ==> rot13 (rot13 s) == s

-- prop_rot13_once :: [Char] -> Property
-- prop_rot13_once s = not (null s) ==> isAlphabetic s ==> rot13 s /= s

main :: IO ()
main = do
    print (rot13 "abc def")
    print (rot13 "nop qrs")