import Data.List
import LTS
import Test.QuickCheck
import Exercise1

genLabel :: Gen Label
genLabel = do
    char <- elements ['a'..'z']
    return [char]

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

genRandomTransition :: [State] -> [Label] -> [Label] -> State -> Int -> Gen [(State, Label, State)]
genRandomTransition _ _ _ _ 0 = return []
genRandomTransition q li lu s n = do
    q1 <- elements q
    q2 <- elements q
    reaction <- elements (li ++ lu)
    rest <- genRandomTransition q li lu q1 (n - 1)
    return ((q1, reaction, q2) : rest)

getInvalidTransition :: [State] -> [Label] -> [Label] -> State -> Int -> Gen [(State, Label, State)]
getInvalidTransition _ _ _ _ 0 = return []
getInvalidTransition q li lu s n = do
    q1 <- choose (100,200)
    q2 <- choose (100,200)
    reaction <- elements (li ++ lu)
    rest <- getInvalidTransition q li lu q1 (n - 1)
    return ((q1, reaction, q2) : rest)

genIOLTSRandom :: Gen IOLTS
genIOLTSRandom = do
    n <- choose (0, 10)
    let q = [0..n]
    (li, lu) <- genLabels
    m <- choose (1, 20)
    q0 <- elements q
    t <- genRandomTransition q li lu q0 m
    return (q, li, lu, t, q0)

genIOLTSLinearIO :: Gen IOLTS
genIOLTSLinearIO = do
    n <- choose (0, 10)
    let q = [0..n]
    (li, lu) <- genLabels
    m <- choose (1, 20)
    q0 <- elements q
    t <- genLinearInOutTransition q li lu q0 m
    return (q, li, lu, t, q0)

genIOLTSLinear :: Gen IOLTS
genIOLTSLinear = do
    n <- choose (0, 10)
    let q = [0..n]
    (li, lu) <- genLabels
    m <- choose (1, 20)
    q0 <- elements q
    t <- genLinearTransition q li lu q0 m
    return (q, li, lu, t, q0)

genIOLTSInvalid :: Gen IOLTS
genIOLTSInvalid = do
    n <- choose (0, 10)
    let q = [0..n] ++ [0..n]
    (li, lu) <- genLabels
    m <- choose (1, 20)
    q0 <- choose (11, 20)
    t <- getInvalidTransition q li lu q0 m
    return (q, li, li, t, q0)

main :: IO ()
main = do
    print ("prop_startStateValid")
    quickCheck (forAll genIOLTSRandom prop_startStateValid)
    quickCheck (forAll genIOLTSLinearIO prop_startStateValid)
    quickCheck (forAll genIOLTSLinear prop_startStateValid)
    quickCheck (forAll genIOLTSInvalid prop_startStateValid)

    print ("prop_inputListEmpty")
    quickCheck (forAll genIOLTSRandom prop_inputListEmpty)
    quickCheck (forAll genIOLTSLinearIO prop_inputListEmpty)
    quickCheck (forAll genIOLTSLinear prop_inputListEmpty)
    quickCheck (forAll genIOLTSInvalid prop_inputListEmpty)

    print ("prop_outputListEmpty")
    quickCheck (forAll genIOLTSRandom prop_outputListEmpty)
    quickCheck (forAll genIOLTSLinearIO prop_outputListEmpty)
    quickCheck (forAll genIOLTSLinear prop_outputListEmpty)
    quickCheck (forAll genIOLTSInvalid prop_outputListEmpty)

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