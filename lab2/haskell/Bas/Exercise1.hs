module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

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
invalidTransitions (q, li, lu, [], q0) = False
invalidTransitions (q, li, lu, ((stateOne, label, stateTwo):xs), q0)
    | not (elem stateOne q) = True
    | not ((elem label li) || (elem label lu)) = True
    | not (elem stateTwo q) = True
    | otherwise = invalidTransitions (q, li, lu, xs, q0)

prop_startStateValid :: IOLTS -> Property
prop_startStateValid (q, li, lu, t, q0) = property (not (null q))

prop_inputListEmpty :: IOLTS -> Property
prop_inputListEmpty (q, li, lu, t, q0) = property (not (null li))

prop_outputListEmpty :: IOLTS -> Property
prop_outputListEmpty (q, li, lu, t, q0) = property (not (null lu))

prop_uniqueStates :: IOLTS -> Property
prop_uniqueStates (q, li, lu, t, q0) = property (not (length q /= length (nub q)))

prop_uniqueInputs :: IOLTS -> Property
prop_uniqueInputs (q, li, lu, t, q0) = property (not (length li /= length (nub li)))

prop_uniqueOutputs :: IOLTS -> Property
prop_uniqueOutputs (q, li, lu, t, q0) = property (not (length lu /= length (nub lu)))

prop_inputOutputUnique :: IOLTS -> Property
prop_inputOutputUnique (q, li, lu, t, q0) = property (length (intersect li lu) == 0)

prop_transitionsValid :: IOLTS -> Property
prop_transitionsValid (q, li, lu, t, q0) = property (not(invalidTransitions (q, li, lu, t, q0)))

prop_startInStates :: IOLTS -> Property
prop_startInStates (q, li, lu, t, q0) = property (q0 `elem` q)

main :: IO ()
main = do
    print tretmanR2
    print (validateLTS tretmanR1)

    -- List of factors that invalidate an IOLTS
    -- 1. Start state not in list of states
    -- 2. Empty list of states
    -- 3. Transitions to states that aren't in state list
    -- 4. Duplicate labels
    -- 5. Duplicate states
    -- 6. The next transition doesn't continue on previous next state
    -- 7. Input labels and output labels overlap eachother