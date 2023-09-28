module Exercise1 where

import Data.List
import LTS

-- Validation function for (IO)LTS: Labeled Transition System
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

-- Function that checks if the transitions are valid
invalidTransitions :: IOLTS -> Bool
invalidTransitions (q, li, lu, [], q0) = False
invalidTransitions (q, li, lu, ((stateOne, label, stateTwo):xs), q0)
    | not (elem stateOne q) = True
    | not ((elem label li) || (elem label lu)) = True
    | not (elem stateTwo q) = True
    | otherwise = invalidTransitions (q, li, lu, xs, q0)

-- Property: Checks if start state is valid
prop_startStateValid :: IOLTS -> Property
prop_startStateValid (q, li, lu, t, q0) = property (not (null q))

-- Property: Checks if input labels are valid
prop_inputLabelsEmpty :: IOLTS -> Property
prop_inputLabelsEmpty (q, li, lu, t, q0) = property (not (null li))

-- Property: Checks if output labels are valid
prop_outputLabelsEmpty :: IOLTS -> Property
prop_outputLabelsEmpty (q, li, lu, t, q0) = property (not (null lu))

-- Property: Checks if all states are unique (no duplicates)
prop_uniqueStates :: IOLTS -> Property
prop_uniqueStates (q, li, lu, t, q0) = property (not (length q /= length (nub q)))

-- Property: Checks if input labels are unique (no duplicates)
prop_uniqueInputs :: IOLTS -> Property
prop_uniqueInputs (q, li, lu, t, q0) = property (not (length li /= length (nub li)))

-- Property: Checks if output labels are unique (no duplicates)
prop_uniqueOutputs :: IOLTS -> Property
prop_uniqueOutputs (q, li, lu, t, q0) = property (not (length lu /= length (nub lu)))

-- Property: Checks if input labels don't share common elements with output labels
prop_inputOutputUnique :: IOLTS -> Property
prop_inputOutputUnique (q, li, lu, t, q0) = property (length (intersect li lu) == 0)

-- Property: Checks if the transitions are valid (if they don't go to a unknown state and so on)
prop_transitionsValid :: IOLTS -> Property
prop_transitionsValid (q, li, lu, t, q0) = property (not(invalidTransitions (q, li, lu, t, q0)))

-- Property: Checks if start state exists
prop_startInStates :: IOLTS -> Property
prop_startInStates (q, li, lu, t, q0) = property (q0 `elem` q)


-- List of factors that invalidate an IOLTS
-- 1. Start state not in list of states
-- 2. Empty list of states
-- 3. Transitions to states that aren't in state list
-- 4. Duplicate labels
-- 5. Duplicate states
-- 6. The next transition doesn't continue on previous next state
-- 7. Input labels and output labels overlap eachother


-- Time spend: 3 hours