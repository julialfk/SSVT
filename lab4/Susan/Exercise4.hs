module Exercise4 where
import Test.QuickCheck
import Control.Monad

type Rel a = [(a,a)]

main :: IO ()
main = do
    quickCheck (forAll genNonEmptyDomain prop_EmptyList)
    quickCheck (forAll genReflexiveDomainsAndRelations prop_ReflexiveRelation)

-- Write a function for checking whether a relation is serial:
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain relation = all (\x -> any (\(x', y) -> x' == x) relation) domain

-- Any non-empty domain under an empty relation, should return false
prop_EmptyList :: Eq a => [a] -> Property
prop_EmptyList domain = property ((isSerial domain []) == False)

prop_ReflexiveRelation :: ([Int], [(Int, Int)]) -> Property
prop_ReflexiveRelation (domain, relation) = property ((isSerial domain relation) == True)

genNonEmptyDomain :: Gen [Int]
genNonEmptyDomain = listOf arbitrary `suchThat` (not . null)

genArbitraryDomain :: Gen [Int]
genArbitraryDomain = listOf arbitrary

genReflexiveRelation :: [Int] -> Gen [(Int, Int)]
genReflexiveRelation domain = do
    pairs <- forM domain (\x -> return (x, x))
    return pairs

genReflexiveDomainsAndRelations :: Gen ([Int], [(Int, Int)])
genReflexiveDomainsAndRelations = do
    domain <- genArbitraryDomain
    relation <- genReflexiveRelation domain
    return (domain, relation)

-- Consider the relation R = {(x, y) | x = y(mod n)}, where (mod n) is the modulo function
-- in modular arithmetic and n > 0. Discuss whether (and when) R is serial. How can you
-- test whether R is serial? How can you prove that R is serial?

