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

propROT13 :: [Char] -> Bool
propROT13 s = rot13 (rot13 s) == s

main :: IO ()
main = do
    print (rot13 "abc def")
    print (rot13 "nop qrs")
    quickCheck propROT13

-- First print shows that rot13, changes order of charSize
-- QuickCheck used to see if rotating twice also gives back
-- same string.
