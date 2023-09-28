import Data.List
import LTS
import Test.QuickCheck

--type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

-- genLabel :: Gen Label
-- genLabel = elements ['a'..'z']

--q, li, lu, t, q0
genIOLTS :: Gen ([State], [Label], [Label], [LabeledTransition], State)
genIOLTS = do
    n <- choose (0, 10)
    q <- [0..n]
    li <- elements ['a'..'z']
    lu <- elements ['a'..'z']
    -- li <- genLabel
    -- lu <- genLabel
    t <- []
  --q0 <- (shuffle q) !! 0
    q0 <- elements q
    return (q, [li], [lu], t, q0)

positiveIntListGenerator :: Gen [Int]
positiveIntListGenerator = vectorOf 100 (arbitrary `suchThat` (\x -> x > 0))

randomList :: IO [Int]
randomList = generate positiveIntListGenerator



