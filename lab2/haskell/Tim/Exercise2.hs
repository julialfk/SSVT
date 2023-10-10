module Exercise2 where

import Data.List
import LTS
import Test.QuickCheck
import Exercise1

-- Generator: Generates label for the LTS
genLabel :: Gen Label
genLabel = do
    char <- elements ['a'..'z']
    return [char]

-- Generator: Generates input and output labels for the LTS
-- Chooses random number of labels from 1 to 5, generates this number of random
-- generated labels using genLabel. Then checks foor duplicates within the list
-- And uniqueness in both lists.
genLabels :: Gen ([Label], [Label])
genLabels = do
    n <- choose (1, 5)
    li <- vectorOf n genLabel
    m <- choose (1, 5)
    lu <- vectorOf m genLabel

    let li' = nub li
        lu' = nub lu
        commonElements = li' `intersect` lu'

    if not (null commonElements)
        then genLabels
        else return (li', lu')

-- Generator: Generates random transitions for LTS
-- All transition generators work about the same; take a random begin state, then a random next state
-- pick a random input or output label. Do this until the number of transition set by n have been 
-- generated.
genRandomTransition :: [State] -> [Label] -> [Label] -> State -> Int -> Gen [(State, Label, State)]
genRandomTransition _ _ _ _ 0 = return []
genRandomTransition q li lu s n = do
    q1 <- elements q
    q2 <- elements q
    reaction <- elements (li ++ lu)
    rest <- genRandomTransition q li lu q1 (n - 1)
    return ((q1, reaction, q2) : rest)

-- Generator: Generates linear (not a tree, but linked-list like) transitions for LTS that rotates the input and output
genLinearInOutTransition :: [State] -> [Label] -> [Label] -> State -> Int -> Gen [(State, Label, State)]
genLinearInOutTransition _ _ _ _ 0 = return []
genLinearInOutTransition q li lu s n = do
    q1 <- elements q
    reaction <- if even n then elements li else elements lu
    rest <- genLinearInOutTransition q li lu q1 (n - 1)
    return ((s, reaction, q1) : rest)

-- Generator: Generates linear (not a tree, but linked-list like) transitions for LTS
genLinearTransition :: [State] -> [Label] -> [Label] -> State -> Int -> Gen [(State, Label, State)]
genLinearTransition _ _ _ _ 0 = return []
genLinearTransition q li lu s n = do
    q1 <- elements q
    reaction <- elements (li ++ lu)
    rest <- genLinearTransition q li lu q1 (n - 1)
    return ((s, reaction, q1) : rest)

-- Generator: Generates invalid transitions, that are not possible
-- Invalid part: the chosen states are not in the states provided,
genInvalidTransition :: [State] -> [Label] -> [Label] -> State -> Int -> Gen [(State, Label, State)]
genInvalidTransition _ _ _ _ 0 = return []
genInvalidTransition q li lu s n = do
    q1 <- choose (100,200)
    q2 <- choose (100,200)
    reaction <- elements (li ++ lu)
    rest <- genInvalidTransition q li lu q1 (n - 1)
    return ((q1, reaction, q2) : rest)

-- Generator: Generates random IOLTS
-- The gen IOLTS also works the same for all generators, with
-- slight differences. First generate the amount of states to be made, then fill
-- a list from stae 0 to n. Generate all labels using the genLabels function. Next
-- Randomly pick how many transitions should be made and generate these as well.
genIOLTSRandom :: Gen IOLTS
genIOLTSRandom = do
    n <- choose (0, 10)
    let q = [0..n]
    (li, lu) <- genLabels
    m <- choose (1, 20)
    q0 <- elements q
    t <- genRandomTransition q li lu q0 m
    return (q, li, lu, t, q0)

-- Generator: Generates linear IOLTS with rotating input and output
genIOLTSLinearIO :: Gen IOLTS
genIOLTSLinearIO = do
    n <- choose (0, 10)
    let q = [0..n]
    (li, lu) <- genLabels
    m <- choose (1, 20)
    q0 <- elements q
    t <- genLinearInOutTransition q li lu q0 m
    return (q, li, lu, t, q0)

-- Generator: Generates linear IOLTS
genIOLTSLinear :: Gen IOLTS
genIOLTSLinear = do
    n <- choose (0, 10)
    let q = [0..n]
    (li, lu) <- genLabels
    m <- choose (1, 20)
    q0 <- elements q
    t <- genLinearTransition q li lu q0 m
    return (q, li, lu, t, q0)

-- Generator: Generates IOLTS that are not valid
-- The invalid parts here are: duplicate states, invalid starting state
-- and input labels are the exact same as the output labels.
genIOLTSInvalid :: Gen IOLTS
genIOLTSInvalid = do
    n <- choose (0, 10)
    let q = [0..n] ++ [0..n]
    (li, lu) <- genLabels
    m <- choose (1, 20)
    q0 <- choose (11, 20)
    t <- genInvalidTransition q li lu q0 m
    return (q, li, li, t, q0)

-- Run all tests
main :: IO ()
main = do
    print ("prop_startStateValid")
    quickCheck (forAll genIOLTSRandom prop_startStateValid)
    quickCheck (forAll genIOLTSLinearIO prop_startStateValid)
    quickCheck (forAll genIOLTSLinear prop_startStateValid)
    quickCheck (forAll genIOLTSInvalid prop_startStateValid)

    print ("prop_inputLabelsEmpty")
    quickCheck (forAll genIOLTSRandom prop_inputLabelsEmpty)
    quickCheck (forAll genIOLTSLinearIO prop_inputLabelsEmpty)
    quickCheck (forAll genIOLTSLinear prop_inputLabelsEmpty)
    quickCheck (forAll genIOLTSInvalid prop_inputLabelsEmpty)

    print ("prop_outputLabelsEmpty")
    quickCheck (forAll genIOLTSRandom prop_outputLabelsEmpty)
    quickCheck (forAll genIOLTSLinearIO prop_outputLabelsEmpty)
    quickCheck (forAll genIOLTSLinear prop_outputLabelsEmpty)
    quickCheck (forAll genIOLTSInvalid prop_outputLabelsEmpty)

    print ("prop_uniqueStates")
    quickCheck (forAll genIOLTSRandom prop_uniqueStates)
    quickCheck (forAll genIOLTSLinearIO prop_uniqueStates)
    quickCheck (forAll genIOLTSLinear prop_uniqueStates)
    quickCheck (forAll genIOLTSInvalid prop_uniqueStates)

    print ("prop_uniqueInputs")
    quickCheck (forAll genIOLTSRandom prop_uniqueInputs)
    quickCheck (forAll genIOLTSLinearIO prop_uniqueInputs)
    quickCheck (forAll genIOLTSLinear prop_uniqueInputs)
    quickCheck (forAll genIOLTSInvalid prop_uniqueInputs)

    print ("prop_uniqueOutputs")
    quickCheck (forAll genIOLTSRandom prop_uniqueOutputs)
    quickCheck (forAll genIOLTSLinearIO prop_uniqueOutputs)
    quickCheck (forAll genIOLTSLinear prop_uniqueOutputs)
    quickCheck (forAll genIOLTSInvalid prop_uniqueOutputs)

    print ("prop_inputOutputUnique")
    quickCheck (forAll genIOLTSRandom prop_inputOutputUnique)
    quickCheck (forAll genIOLTSLinearIO prop_inputOutputUnique)
    quickCheck (forAll genIOLTSLinear prop_inputOutputUnique)
    quickCheck (forAll genIOLTSInvalid prop_inputOutputUnique)

    print ("prop_transitionsValid")
    quickCheck (forAll genIOLTSRandom prop_transitionsValid)
    quickCheck (forAll genIOLTSLinearIO prop_transitionsValid)
    quickCheck (forAll genIOLTSLinear prop_transitionsValid)
    quickCheck (forAll genIOLTSInvalid prop_transitionsValid)

    print ("prop_startInStates")
    quickCheck (forAll genIOLTSRandom prop_startInStates)
    quickCheck (forAll genIOLTSLinearIO prop_startInStates)
    quickCheck (forAll genIOLTSLinear prop_startInStates)
    quickCheck (forAll genIOLTSInvalid prop_startInStates)

-- Time spend: 8 hours

-- We tested the following properties:
-- 1. Start state is not null
-- 2. Not an empty list of input labels
-- 3. Not an empty list of output labels
-- 4. No transitions to states that aren't in state list
-- 5. No duplicate labels
-- 6. No duplicate states
-- 7. The next transition doesn't continue on previous next state
-- 8. Input labels and output labels overlap eachother
-- 9. Start state should be in state lists
--
-- We used multiple generators:
-- Linear transitions generator
-- Linear transitions generator with input then output in states
-- Completely random generator
-- Completely invalid generator
-- 
-- The invalid generators all fail on the following properties:
--No duplicate states
-- No duplicate labels
-- No transitions to states that aren't in state list