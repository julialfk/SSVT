module Exercise4 where

import Data.List
import LTS
import Test.QuickCheck

after :: IOLTS -> Trace -> [State]
iolts `after` ls = path ts [s] ls
    where (ss, is, os, ts, s) = iolts

-- Recursively go through the full path and output all resulting end states.
path :: [LabeledTransition] -> [State] -> Trace -> [State]
path _ ss [] = ss
path ts ss (l:ls) = path ts ss' ls
    where ss' = step ts ss l

-- Get all next states starting from a list of states resulting from a certain label
step :: [LabeledTransition] -> [State] -> Label -> [State]
step ts ss l = nub (foldr (\s acc -> next ts s l ++ acc) [] ss)

-- Get the next states resulting from a certain label
-- Filters out the delta, because the output state after a delta will always be the input state.
next :: [LabeledTransition] -> State -> Label -> [State]
next ts s l = [s2 | (s1, l', s2) <- ts, s1 == s, l' == l, l' /= delta]

-- nextTau :: [LabeledTransition] -> State -> [State]
-- nextTau ts s = map (\(_, _, s') -> s') ts'
--     where ts' = filter (\(s1, l, s2) -> s1 == s && l == tau) ts

-- Check for each end state whether it is can be a result of the last label
propLasts :: IOLTS -> [State] -> Trace -> Bool
propLasts (_, _, _, ts, s0) ss [] = all (\s -> s == s0 || any (\(_, _, s2) -> s == s2) ts') ss
    where ts' = filter (\(s1, l, s2) -> s1 == s0 && l == tau) ts
propLasts (_, _, _, ts, _) ss ls = all (\s -> checkResult ts s (last ls)) ss

checkResult :: [LabeledTransition] -> State -> Label -> Bool
checkResult ts s l = any (\(_, l', s2) -> s == s2 && l' == l) ts

-- Check if the next state after a tau transition from all end states are also included.
propTau :: IOLTS -> [State] -> Bool
propTau (_, _, _, ts, _) ss = foldr (\s acc -> checkNext ts ss s && acc) True ss

checkNext :: [LabeledTransition] -> [State] -> State -> Bool
checkNext ts ss s = all (\(_, _, s2') -> any (== s2') ss) ts'
    where ts' = filter (\(s1, l, s2) -> s1 == s && l == tau) ts

main :: IOLTS -> Bool
main iolts = checkLasts iolts afters lss && checkTau iolts afters
                -- && checkLasts iolts safters slss && checkTau iolts safters
    where 
        (ss, is, os, ts, s) = iolts
        lss = traces (createLTS ts)
        -- slss = straces iolts
        afters = map (after iolts) lss
        -- safters = map (after iolts) slss

checkLasts :: IOLTS -> [[State]] -> [Trace] -> Bool
checkLasts _ [] [] = True
checkLasts _ sss [] = error "More end state sets than traces"
checkLasts _ [] lss = error "More traces than end state sets"
checkLasts iolts (ss:sss) (ls:lss) = propLasts iolts ss ls && checkLasts iolts sss lss

checkTau :: IOLTS -> [[State]] -> Bool
checkTau iolts sss = foldr (\ss acc -> propTau iolts ss && acc) True sss
