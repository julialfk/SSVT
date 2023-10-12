module Exercise4 where
import Test.QuickCheck
import Control.Monad
import Data.Set (fromList, toList)
import Data.List

type Rel a = [(a,a)]

main :: IO ()
main = do
    quickCheck (forAll genNonEmptyDomain prop_EmptyList)
    quickCheck (forAll genReflexiveDomainsAndRelations prop_ReflexiveRelation)
    quickCheck (forAll genModuloDomainsAndRelations prop_ModuloRelationIsSerial)

-- Write a function for checking whether a relation is serial:
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain relation = all (\x -> any (\(x', y) -> x' == x) relation) domain

-- Any non-empty domain under an empty relation, should return false. Input for this function
-- should come from 'G=genNonEmptyDomain'
prop_EmptyList :: Eq a => [a] -> Property
prop_EmptyList domain = property ((isSerial domain []) == False)

-- Reflexive relations, by nature, are always serial. Input for this function should
-- come from 'genReflexiveDomainsAndRelations'
prop_ReflexiveRelation :: ([Int], [(Int, Int)]) -> Property
prop_ReflexiveRelation (domain, relation) = property ((isSerial domain relation) == True)

-- Generates a non-empty domain
genNonEmptyDomain :: Gen [Int]
genNonEmptyDomain = listOf arbitrary `suchThat` (not . null)

-- Generates an arbitrary domain (can be empty)
genArbitraryDomain :: Gen [Int]
genArbitraryDomain = listOf arbitrary

-- Given a domain, generate a relation that consists of all the identities for its members.
-- So the domain [3, 0, 2] would generate the relation [(3,3), (0,0), (2,2)]
genReflexiveRelation :: [Int] -> Gen [(Int, Int)]
genReflexiveRelation domain = do
    pairs <- forM domain (\x -> return (x, x))
    return pairs

-- Generates both an arbitrary domain and its corresponding reflexive relation.
-- Does so by calling genArbitraryDomain and genReflexiveRelation
genReflexiveDomainsAndRelations :: Gen ([Int], [(Int, Int)])
genReflexiveDomainsAndRelations = do
    domain <- genArbitraryDomain
    relation <- genReflexiveRelation domain
    return (domain, relation)

-- Consider the relation R = {(x, y) | x = y(mod n)}, where (mod n) is the modulo function
-- in modular arithmetic and n > 0. Discuss whether (and when) R is serial. How can you
-- test whether R is serial? How can you prove that R is serial?

-- WHETHER AND WHEN R IS SERIAL
-- Modulo is defined as the non-negative remainder after division. This means that x and y will never be negative.
-- This means that per definition, R cannot be serial if the domain includes negative numbers, because it violates the rules of R.
-- However, if the domain is all positive integers, then R is serial. [EXPLAIN INTUITION]

-- PROOF THAT IT IS SERIAL
-- We can formally prove this [SHOW FORMAL PROOF]

-- TEST THAT IT IS SERIAL
-- We can test this by using our isSerial function, in combination with a generator. This generator accepts
-- a domain with non-negative numbers, and then computes all the corresponding (x,y) pairs as defined by the modulo.

genNonNegativeDomain :: Gen [Int]
genNonNegativeDomain = do
    let genInt = (arbitrary :: Gen Int) `suchThat` (> 0)
    l <- choose(2, 150) -- explain de 200 (was eerst 100 maar door nub verlies je weer waardes), denk aan performance
    domain <- vectorOf l genInt
    let uDomain = nub domain
    if length uDomain < 2 then genNonNegativeDomain else return uDomain


-- Given a domain, this function creates the modulo relation as defined in the exercise. For n we use [1..maximum domain],
-- where maximum domain is the largest number in the domain. We picked this over maxBound, because 
moduloRelation :: [Int] -> [(Int, Int)]
moduloRelation domain = removeDuplicates [(x,y) | x <- domain, y <- domain, n <- [1..(maximum domain)], y `mod` n == x `mod` n]


genModuloDomainsAndRelations :: Gen ([Int], [(Int, Int)])
genModuloDomainsAndRelations = do
    domain <- genNonNegativeDomain
    let relation = moduloRelation domain
    return (domain, relation)

-- Tests whether modulo relations are serial. Input of this function should come from 'genModuleDomainsAndRelations'
prop_ModuloRelationIsSerial :: ([Int], [(Int, Int)]) -> Property
prop_ModuloRelationIsSerial (domain, relation) = property ((isSerial domain relation) == True)

-- Removes duplicates from a list by turning it into a set and then back to a list
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = toList . fromList
