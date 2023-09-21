module Exercise6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture2
import Lecture3
import SetOrd

-- Take a Form and output an equivalent CNF Form.
-- This algorithm follows the given solution process for Question 13 from Workshop 3.
-- First, a truth table is created, after which the valuations resulting in
-- False are filtered out. If no rows are left, the first property in the
-- truth table is selected to output 'Prop 1 || not Prop 1'.
-- Otherwise, one big conjunction is created out of disjunctions from the
-- truth table rows, where all properties are in the False form.
cnf :: Form -> Form
cnf f = if vals == []
            then 
                Dsj [Prop p, Neg (Prop p)]
            else 
                Cnj (disjuncate (vals))
    where
        tt = truthTable f
        (v:vs) = map snd tt
        vals = map snd (getFalse tt)
        p = fst (head v)

-- Create a truthtable from the expression by getting all possible valuations
-- and evaluate all combinations.
-- The results of the evaluations are tupled with the valuations.
truthTable :: Form -> [(Bool, Valuation)]
truthTable f = map (\x -> ((evl x f), x)) (allVals f)

-- Filter out all combinations that produce a False outcome.
getFalse :: [(Bool, Valuation)] -> [(Bool, Valuation)]
getFalse = filter (\(b, _) -> not b)

-- Convert a boolean assignment to a property name into an actual property in
-- such a way that the resulting form results in False.
negateProp :: (Name, Bool) -> Form
negateProp (p, b) | b = Neg (Prop p)
                  | otherwise = Prop p

-- Create a list of disjunctions from the negative properties.
disjuncate :: [Valuation] -> [Form]
disjuncate xs = map (\x -> Dsj (map negateProp x)) xs

-- time spent: 3 hrs (Julia), 6 hrs (Bas). We took longer because we first wanted to
-- implement all equivalence laws to convert the form into CNF and it took us a while
-- to realize that using truth tables was also an option.
