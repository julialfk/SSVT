module Exercise1 where
import Data.List
import Test.QuickCheck
import SetOrd


-- Generator: Generator to create lists with only duplicate elements
-- We use this generator to generate duplicate lists for the specific property
genSet :: Gen Set Int
genSet = do
  n <- choose (2, 25)
  elements <- vectorOf n arbitrary
  return list2set elements


-- Implement a random data generator for the datatype  Set Int ,
-- where  Set  is as defined in SetOrd.hs. First do this from scratch,
-- next give a version that uses QuickCheck to random test this datatype
