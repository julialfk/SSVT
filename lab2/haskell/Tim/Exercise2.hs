import Data.List
import LTS
import Test.QuickCheck

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
    if not (null (lu \\ li))
        then return (li, lu)
        else genLabels

genLinearInOutTransition :: [State] -> [Label] -> [Label] -> State -> Int -> Gen [(State, Label, State)]
genLinearInOutTransition _ _ _ _ 0 = return []
genLinearInOutTransition q li lu s n = do
    q1 <- elements q
    reaction <- if even n then elements li else elements lu
    rest <- genLinearInOutTransition q li lu q1 (n - 1)
    return ((s, reaction, q1) : rest)

genLinearTransition :: [State] -> [Label] -> [Label] -> State -> Int -> Gen [(State, Label, State)]
genLinearTransition _ _ _ _ 0 = return []
genLinearTransition q li lu s n = do
    q1 <- elements q
    reaction <- elements (li ++ lu)
    rest <- genLinearTransition q li lu q1 (n - 1)
    return ((s, reaction, q1) : rest)

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
    putStrLn "Sample IOLTS:"
    samples <- generate (vectorOf 5 genIOLTSRandom)
    mapM_ print samples