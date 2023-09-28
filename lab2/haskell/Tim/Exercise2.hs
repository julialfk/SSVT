import Data.List
import LTS
import Test.QuickCheck

genLabel :: Gen Label
genLabel = do
    char <- elements ['a'..'z']
    return [char]

generateLabels :: Gen ([Label], [Label])
generateLabels = do
    n <- choose (1, 5)
    li <- vectorOf n genLabel
    m <- choose (1, 5)
    lu <- vectorOf m genLabel
    if not (null (lu \\ li))
        then return (li, lu)
        else generateLabels

genIOLTS :: Gen IOLTS
genIOLTS = do
    n <- choose (0, 10)
    let q = [0..n]
    (li, lu) <- generateLabels
    --transitions here
    q0 <- elements q
    return (q, li, lu, [], q0)

main :: IO ()
main = do
    putStrLn "Sample IOLTS:"
    samples <- generate (vectorOf 5 genIOLTS)  -- Generate 5 samples
    mapM_ print samples