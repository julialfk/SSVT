module Exercise2 where

import Data.List
import Test.QuickCheck
import SetOrd
import Exercise1

-- Additional generator that creates smaller sets with a smaller value range,
-- so that QuickCheck does not discard too many sets for prop_Identity.
genSmallSet :: Gen (Set Int)
genSmallSet = do
    n <- choose (1, 2)
    content <- vectorOf n (suchThat arbitrary (\x -> x >= 0 && x <= 3))
    return (list2set content)

-- Takes a generator function and operation function and uses it to create a tuple
-- with two newly generated sets and the output set from the operation.
genNewSet :: Gen (Set Int) -> (Set Int -> Set Int -> Set Int) -> Gen (Set Int, Set Int, Set Int)
genNewSet gen f = do
    xs <- gen
    ys <- gen
    return (xs, ys, f xs ys)

-- Creates an intersection from two sets by filtering elements found in ys from xs.
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = Set (filter (`elem` ys) xs)

-- Creates a union from two sets by concatenating xs and ys and removing duplicates.
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set xs) (Set ys) = Set (nub (xs ++ ys))

-- Creates a difference from two sets by filtering elements not found in ys from xs.
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set xs) (Set ys) = Set (filter (`notElem` ys) xs)

-- Property that checks whether all elements in zs are in both xs and ys (i.e. a subset of both xs and ys).
-- This should be true for the union.
prop_InBothSets :: Ord a => (Set a, Set a, Set a) -> Bool
prop_InBothSets (Set xs, Set ys, Set zs) = all (\z -> z `elem` xs && z `elem` ys) zs

-- Property that checks whether all elements in zs are in either xs or ys.
-- This should be true for the union.
prop_InEitherSet :: Ord a => (Set a, Set a, Set a) -> Bool
prop_InEitherSet (Set xs, Set ys, Set zs) = all (`elem` xs ++ ys) zs

-- Property that checks whether all elements in zs are in the first set xs (i.e. zs is a subset of xs).
-- The difference should be a subset of the first original set.
prop_InFstSet :: Ord a => (Set a, Set a, Set a)-> Bool
prop_InFstSet (Set xs, _, Set zs) = all (`elem` xs) zs

-- Property that checks whether no elements in zs are in the second set ys.
-- The difference should not have any elements from the second set.
prop_NotInSndSet :: Ord a => (Set a, Set a, Set a) -> Bool
prop_NotInSndSet (_, Set ys, Set zs) = all (`notElem` ys) zs

-- Property that checks whether the size of zs is greater than/equal to both xs and ys.
-- This should be true for the union, since merging two sets cannot result into a smaller set.
prop_LargerEqualSources :: Ord a => (Set a, Set a, Set a) -> Bool
prop_LargerEqualSources (Set xs, Set ys, Set zs) = lenzs >= length xs && lenzs >= length ys
    where lenzs = length zs

-- Property that checks whether the size of zs is smaller than/equal to both xs and ys.
-- This should be true for the intersection, since the intersection is a subset of both sets.
prop_SmallerEqualSources :: Ord a => (Set a, Set a, Set a) -> Bool
prop_SmallerEqualSources (Set xs, Set ys, Set zs) = lenzs <= length xs && lenzs <= length ys
    where lenzs = length zs

-- Property that checks whether the size of zs is smaller than/equal to xs.
-- This should be true for the difference, since the difference is a subset of xs.
prop_SmallerEqualSource :: Ord a => (Set a, Set a, Set a) -> Bool
prop_SmallerEqualSource (Set xs, _, Set zs) = length zs <= length xs

-- Property that checks whether the size of zs is smaller than/equal to xs and ys combined.
-- This should be true for the union, since the merger of two sets cannot have more elements than the two sets combined.
prop_SmallerEqualTotal :: Ord a => (Set a, Set a, Set a) -> Bool
prop_SmallerEqualTotal (Set xs, Set ys, Set zs) = length zs <= length xs + length ys

-- Property that checks whether the output set has any duplicates.
-- This should be true for all sets.
prop_NoDupes :: Ord a => (Set a, Set a, Set a) -> Bool
prop_NoDupes (_, _, Set zs) = length zs == length (nub zs)

-- Property that checks whether the two original sets are the same when the size of output set is the same as the size of both xs and ys.
-- This should be true for the intersection and union.
prop_Identity :: Ord a => (Set a, Set a, Set a) -> Property
prop_Identity (Set xs, Set ys, Set zs) = lenzs == length xs && lenzs == length ys ==> xs == ys
    where lenzs = length zs

-- Property that checks whether xs and ys have no overlap when the size of output set is the same xs.
-- This should be true for the difference.
prop_Exclusive :: Ord a => (Set a, Set a, Set a) -> Property
prop_Exclusive (Set xs, Set ys, Set zs) = length zs == length xs ==> all (`notElem` ys) xs

checkIntersection :: [Set Int] -> [Set Int] -> [Set Int] -> IO ()
checkIntersection [] [] [] = putStrLn "+++ OK, passed 100 Intersect tests."
checkIntersection (x:xs) (y:ys) (z:zs) | prop_InBothSets (x, y, z) &&
                                         prop_SmallerEqualSources (x, y, z) &&
                                         prop_NoDupes (x, y, z) = checkIntersection xs ys zs
                                       | otherwise = putStrLn "Error: setIntersection did not pass all tests!"
checkIntersection _ _ _ = putStrLn "Error: checkIntersection input lists do not have equal lengths!"

checkUnion :: [Set Int] -> [Set Int] -> [Set Int] -> IO ()
checkUnion [] [] [] = putStrLn "+++ OK, passed 100 Union tests."
checkUnion (x:xs) (y:ys) (z:zs) | prop_InEitherSet (x, y, z) &&
                                  prop_LargerEqualSources (x, y, z) &&
                                  prop_SmallerEqualTotal (x, y, z) &&
                                  prop_NoDupes (x, y, z) = checkUnion xs ys zs
                                | otherwise = putStrLn "Error: setUnion did not pass all tests!"
checkUnion _ _ _ = putStrLn "Error: checkUnion input lists do not have equal lengths!"

checkDifference :: [Set Int] -> [Set Int] -> [Set Int] -> IO ()
checkDifference [] [] [] = putStrLn "+++ OK, passed 100 Difference tests."
checkDifference (x:xs) (y:ys) (z:zs) | prop_InFstSet (x, y, z) &&
                                       prop_NotInSndSet (x, y, z) &&
                                       prop_SmallerEqualSource (x, y, z) &&
                                       prop_NoDupes (x, y, z) = checkDifference xs ys zs
                                     | otherwise = putStrLn "Error: setUnion did not pass all tests!"
checkDifference _ _ _ = putStrLn "Error: checkUnion input lists do not have equal lengths!"


checkProps :: IO()
checkProps = do
    -- Check using our own generator. This only generates a single sample, while QuickCheck
    -- generates a new sample for each property. For this reason, QuickCheck is more thorough.
    -- prop_Identity and prop_Exclusive also use preconditions, which can only be used with QuickCheck,
    -- so there are not checked using our own generator. We could have created a version without the
    -- preconditions, but then we would also need an additional generator that recursively discards
    -- set pairs that do not satisfy the precondition until it finds one that does. For this exercise,
    -- we left out the implementation of this additional test, and just left it at testing it with QuickCheck.
    xs <- coolSample
    ys <- coolSample
    let zipped = zip xs ys
    let intersects = map (uncurry setIntersection) zipped
    let unions = map (uncurry setUnion) zipped
    let differences = map (uncurry setDifference) zipped
    checkIntersection xs ys intersects
    checkUnion xs ys unions
    checkDifference xs ys differences
    
    print "quickChecking Intersection"
    quickCheck (forAll (genNewSet genSet setIntersection) prop_InBothSets)
    quickCheck (forAll (genNewSet genSet setIntersection) prop_SmallerEqualSources)
    quickCheck (forAll (genNewSet genSet setIntersection) prop_NoDupes)
    quickCheck (forAll (genNewSet genSmallSet setIntersection) prop_Identity)

    print "quickChecking Union"
    quickCheck (forAll (genNewSet genSet setUnion) prop_InEitherSet)
    quickCheck (forAll (genNewSet genSet setUnion) prop_LargerEqualSources)
    quickCheck (forAll (genNewSet genSet setUnion) prop_SmallerEqualTotal)
    quickCheck (forAll (genNewSet genSet setUnion) prop_NoDupes)
    quickCheck (forAll (genNewSet genSmallSet setUnion) prop_Identity)

    print "quickChecking Difference"
    quickCheck (forAll (genNewSet genSet setDifference) prop_InFstSet)
    quickCheck (forAll (genNewSet genSet setDifference) prop_NotInSndSet)
    quickCheck (forAll (genNewSet genSet setDifference) prop_SmallerEqualSource)
    quickCheck (forAll (genNewSet genSet setDifference) prop_NoDupes)
    quickCheck (forAll (genNewSet genSet setDifference) prop_Exclusive)
