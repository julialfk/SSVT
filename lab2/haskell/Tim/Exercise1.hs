module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

--type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

-- validateLTS :: IOLTS -> Bool
-- validateLTS (q, li, lu, t, q0) = False if q is empty and not integer
-- validateLTS (q, li, lu, t, q0) = False if L is not countable
-- validateLTS (q, li, lu, t, q0) = False if q0 is not in q
-- validateLTS (q, li, lu, t, q0) = True


validateLTS :: IOLTS -> Bool
validateLTS (q, li, lu, t, q0)
    | null q = False
    | null li = False
    | null lu = False
    | q0 `notElem` q = False
    | otherwise = True

main :: IO ()
main = do
    print tretmanS4
    print (validateLTS tretmanK3)