module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

-- 

--type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

-- validateLTS :: IOLTS -> Bool
-- validateLTS (q, li, lu, t, q0) = False if q is empty and not integer
-- validateLTS (q, li, lu, t, q0) = False if L is not countable
-- validateLTS (q, li, lu, t, q0) = False if q0 is not in q
-- validateLTS (q, li, lu, t, q0) = True

-- Check for duplicates
validateLTS :: IOLTS -> Bool
validateLTS (q, li, lu, t, q0)
    | null q = False
    | null li = False
    | null lu = False
    | length q /= length (nub q) = False
    | length li /= length (nub li) = False
    | length lu /= length (nub lu) = False
    | length (intersect li lu) > 0 = False
    | q0 `notElem` q = False
    | invalidTransitions (q, li, lu, t, q0) = False
    | otherwise = True

invalidTransitions :: IOLTS -> Bool
invalidTransitions (q, li, lu, ((stateOne, label, stateTwo):xs), q0)
    | not (elem stateOne q) = False
    | not ((elem label li) || (elem label lu)) = False
    | not (elem stateTwo q) = False
    | otherwise = invalidTransitions (q, li, lu, xs, q0)


main :: IO ()
main = do
    print tretmanS4
    print (validateLTS tretmanK3)

    -- List of factors that invalidate an IOLTS
    -- 1. Start state not in list of states
    -- 2. Empty list of states
    -- 3. Transitions to states that aren't in state list
    -- 4. Duplicate labels
    -- 5. Duplicate states
    -- 6. The next transition doesn't continue on previous next state
    -- 7. Input labels and output labels overlap eachother