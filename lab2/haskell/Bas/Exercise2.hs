import Data.List
import LTS
import Test.QuickCheck

-- ltsGen :: Gen IOLTS
-- ltsGen = do
--     states <- randomList
--     labels <- randomLabels

positiveIntListGenerator :: Gen [Int]
positiveIntListGenerator = vectorOf 100 (arbitrary `suchThat` (\x -> x > 0))

randomList :: IO [Int]
randomList = generate positiveIntListGenerator



