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

genTransition :: [State] -> [Label] -> [Label] -> State -> Int -> Gen [(State, Label, State)]
genTransition _ _ _ _ 0 = return []
genTransition q li lu s n = do
    q1 <- elements q
    reaction <- if even n then elements li else elements lu
    rest <- genTransition q li lu q1 (n - 1)
    return ((s, reaction, q1) : rest)

genIOLTS :: Gen IOLTS
genIOLTS = do
    n <- choose (0, 10)
    let q = [0..n]
    (li, lu) <- genLabels
    m <- choose (1, 20)
    q0 <- elements q
    t <- genTransition q li lu q0 m
    return (q, li, lu, t, q0)

main :: IO ()
main = do
    putStrLn "Sample IOLTS:"
    samples <- generate (vectorOf 5 genIOLTS)
    mapM_ print samples