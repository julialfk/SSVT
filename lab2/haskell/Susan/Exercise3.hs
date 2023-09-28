module Exercise3 where

import qualified Exercise2 as Ex2
import Data.List
import LTS
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck (forAll genRandomTraces prop_EmptyTrace)
    -- quickCheck (forAll Ex2.genIOLTSOut prop_Subset)
    -- generatedIOLTS <- generate Ex2.genIOLTSOut  -- Generate an IOLTS
    -- let (_, _, _, transitions, _) = generatedIOLTS
    -- print (traces (createLTS transitions))
    -- print(generatedIOLTS)
    -- print (prop_Subset generatedIOLTS)
    -- print (take 100 (straces generatedIOLTS))
    listResult <- generate genDeltaTraces
    -- print (take 100 listResult)
    quickCheck (forAll genDeltaTraces prop_DeltaPresent)


-- 1. Implement a function that returns all suspension traces of a given IOLTS

-- Returns True if the given transition has an output. It has an output if its label
-- is present in the outputList of the IOLTS.
hasOutput :: LabeledTransition -> [Label] -> Bool
hasOutput (_, label, _) outputList = label `elem` outputList

-- Returns True of this state has a delta transition, meaning that it has no output
-- transitions and therefore quiescence can occur.
isDeltaState :: IOLTS -> State -> Bool
isDeltaState (_, _, outputList, transitions, _) state = 
    all (== False) [hasOutput x outputList | x <- filter (\(s, _, _) -> s == state) transitions]

-- Generates a list of all the states in the IOLTS that have a delta transition,
-- and therefore could be part of a suspension trace (i.e. a trace with quiescence).
deltaStates :: IOLTS -> [State]
deltaStates inputIOLTS =
    [state | state <- allStates, isDeltaState inputIOLTS state]
    where
        (allStates, _, _, _, _) = inputIOLTS

-- For the straces implementation, we modified the given traces implementation from LTS.hs
-- The changes that we made was that we add delta transitions to the 'nextTransitions'' function.
-- This is enough to go from traces to straces, because the only difference between them is the presence of delta transitions
-- (straces is a superset of traces). Below in the comments you will see the changes that were made for each function.


-- Add a delta transtion (so a transition from the current state to the current state, by means of delta) to all
-- those states that are defined as 'deltaStates' as per the 'deltaStates' function.
ex3_nextTransitions':: IOLTS -> [LabeledTransition] -> State -> [(State,Label)]
ex3_nextTransitions' inputIOLTS lt q0   | isDeltaState inputIOLTS q0 =  [(s',l) | (s,l,s')<- lt , s == q0] ++ [(q0, delta)]
                                        | otherwise = [(s',l) | (s,l,s')<- lt , s == q0]

-- No changes made here, except for passing along the IOLTS
ex3_findfollowingtransitions':: IOLTS -> [LabeledTransition] -> [State] -> [Label] -> [([State],[Label])]
ex3_findfollowingtransitions' inputIOLTS lt st ls = [(s:st,ls++[l])| (s,l)<-ex3_nextTransitions' inputIOLTS lt (head st)]

-- An extra parameter (IOLTS) is passed along in this function, because it will be needed in the 'ex3_nextTransitions'' function
-- in order to compute the list of delta states.
ex3_traces':: IOLTS -> [LabeledTransition] -> [([State],[Label])]-> [([State],[Label])]
ex3_traces' inputIOLTS lt [] = []
ex3_traces' inputIOLTS lt pairs = pairs ++ ex3_traces' inputIOLTS lt next
    where
        next = concatMap (uncurry $ ex3_findfollowingtransitions' inputIOLTS lt) pairs

-- The type signature of straces is different than the one of traces (IOLTS instead of LTS)
straces :: IOLTS -> [Trace]
straces inputIOLTS = nub $ map snd (ex3_traces' inputIOLTS lt [([q0],[])])
    where
        (q, li, lu, lt, q0) = inputIOLTS

-- What is still left to do here, but what we couldn't figure out how to get done in haskell, is that we have to remove the taus
-- from the straces. We have to remove these, because the trace ['tea', 'tau', 'tea'] is the same trace as ['tea', 'tea'].
-- However, we couldn't figure this out because of the infinite property of the list (this makes it impossible to filter for instance).

-- 3.2: Use your IOLTS generator and your straces function to create a random traces generator for QuickCheck
genRandomTraces :: Gen [Trace]
genRandomTraces = do
    iolt <- Ex2.genIOLTSRandom
    return (straces iolt)

-- We also made a generator that returns the straces of an IOLTS that for sure has delta states (which is usefull for a property we
-- will be testing later)
genDeltaTraces :: Gen [Trace]
genDeltaTraces = do
    iolt <- Ex2.genIOLTSRandom
    if deltaStates iolt == []
        then
            genDeltaTraces
        else
            return (straces iolt)


-- 3.3:  Test your straces function using QuickCheck

-- We've built a generator that generates IOLTSs with finite traces, but could contain deltas (see Exercise 2)
-- All the traces from these IOLTSs should also be present in straces.
-- We initially thought that straces being an infinite list would not be an issue for this property, because the element in traces should
-- for sure be found in it, and not at the very end. However, when running this property, we do notice the program taking forever. Most
-- tests never finish. We have therefore removed this test from the main.
prop_Subset :: IOLTS -> Property
prop_Subset inputIOLTS = property (all (`elem` straces inputIOLTS) finiteTraces)
    where
        (_, _, _, transitions, _) = inputIOLTS
        finiteTraces = traces (createLTS transitions)

-- Another property is that the empty trace should always be part of straces. It should also be the first element
prop_EmptyTrace :: [Trace] -> Property
prop_EmptyTrace randomTraces = property (head(randomTraces) == [])

-- We also wanted to make a generator that created IOLTS that had finite traces (which means no loops), but also
-- no delta states (which means every state has an output). However, this is not possible because when every state has an output,
-- every state must point to something, which means that you will eventually always end up with a loop (and therefore traces is no
-- longer finite). We wanted this generator to test if traces would equal straces for this IOLTS with no delta states. But again,
--  we cannot test this because having no delta states per definition means having a loop, making traces (and with that straces) infinite.

-- If the IOLTS has any delta states, then 'delta' should occur in at least one of the traces.
-- This property is tested with the 'genDeltaTraces' generator (because we do need the IOLTS to have a delta state).
-- ISSUE: tests either fail (when take 200 for instance), or they loop indefinitely (because 'delta' is not found).
-- Ik snap niet waarom er geen delta in zou zitten? We pakken juist heel expliciet straces die wel een delta zouden moeten hebben?
prop_DeltaPresent :: [Trace] -> Property
prop_DeltaPresent traces =  property (any ("delta" `elem`) traces)