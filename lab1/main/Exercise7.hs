module Exercise7 where

import SetOrd
import Test.QuickCheck
import Lecture3

-- Indication of time spent: 4 hours.

main :: IO ()
main = do
      -- Tests whether the length of the sub of a random Prop x is equal to 1.
      -- All tests pass
      quickCheck (forAll genProp prop_BaseCaseLength)

      -- Tests whether the first element of the sub of a random Prop x is equal to x.
      -- All tests pass.
      quickCheck (forAll genProp prop_BaseCaseElement)

      -- Tests whether the number of elements in sub x (where x is a randomly generated Form)
      -- is smaller or equal to the maximum bound of that Form.
      -- All tests pass (computation however isn't instant because of the generator and recursive nature of sub).
      quickCheck (forAll genForm prop_LengthUpperBound)

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)


-- 1: How can you prove that the sub implementation is correct?
-- We could use a proof by induction. The base case would be sub (Prop x), which should return a set consisting of only Prop x.
-- Then for the induction steps of Neg f, Cnj [f1, f2], Dsj [f1, f2], Impl [f1, f2] and Equiv [f1, f2], it should return a set
-- containing the Neg/Dsj/Cnj/Impl/Equiv itself, as well as all the subformulaes in f, f1 and f2.
-- This is similar to Question 3 from Workshop 3.
-- We can then  prove the induction steps by assuming that the base case is true, and then working it out:
-- For Neg f, Cnj [f1, f2], Dsj [f1, f2], Impl [f1, f2] and Equiv [f1, f2], delve deeper and deeping into f, f1 and f2 until you only
-- have Props left. These props are correct because of the base case. Then working your way up again, you get correct subformulae,
-- made up from things that are proven to be correct by the base case.

-- 1. Test the implementation with two QuickCheck properties.
-- We could test the base case, like discussed in the quesiton above. We can see if Prop x
-- indeed returns a set containing only Prop x: so the length of the set has to be 1 and the one element in it has to be x
-- Note: this prop_BaseCaseLength does assume that the implementation of nsub is correct
prop_BaseCaseLength :: Form -> Property
prop_BaseCaseLength x = property (nsub x == 1)

prop_BaseCaseElement :: Form -> Property
prop_BaseCaseElement x = property (firstSetElement (sub x) == x)

-- Another property would be that the number of subformulae is at max a maximum bound, definied by
-- each element of the formula being worth one subformula in itself, combined with the components that it is made
-- up from. See the implementation of upperBound for more details. Again, this property assumes that the implementation
-- of nsub is correct.
prop_LengthUpperBound :: Form -> Property
prop_LengthUpperBound x = property (nsub x <= upperBound x)

-- We also thought about testing whether all the Prop x in the input, are also present in the output (and vice versa).
-- However, we couldn't figure out a way to implement this in Haskell, without using the sub function itself (and this
-- would of course defeat the purpose of testing; you can't test sub with sub itself).
-- We do think that this would be a good property, so we did want to mention it.

-- 2: Write a recursive implementation of the function nsub :: Form -> Int such that nsub f
-- computes the exact number of sub-formulae of the formula f.
nsub :: Form -> Int
nsub x = lengthSet (sub x)

-- Recursively computes the length of of a Set (from the SetOrd module)
lengthSet :: Set a -> Int
lengthSet (Set []) = 0
lengthSet (Set (_:xs)) = 1 + lengthSet (Set xs)

-- Returns the first element (a Form) of a Set Form
firstSetElement :: Set Form -> Form
firstSetElement (Set (x:_)) = x

-- Computes the upper bound of the length of subformulae for a given Form.
-- The upper bound assumes that all subformulae in the formula are unique and
-- thus contribute +1 to the length.
upperBound :: Form -> Int
upperBound (Prop x) = 1
upperBound (Neg f) = upperBound f + 1
upperBound f@(Cnj [f1,f2]) = upperBound f1 + upperBound f2 + 1
upperBound f@(Dsj [f1,f2]) = upperBound f1 + upperBound f2 + 1
upperBound f@(Impl f1 f2) = upperBound f1 + upperBound f2 + 1
upperBound f@(Equiv f1 f2) = upperBound f1 + upperBound f2 + 1

-- Generates a random Form consisting of 1 Prop. This Prop is a non-negative number.
-- Prop is non-negative because negation is done through the Neg Form.
genProp :: Gen Form
genProp = do
  name <- arbitrary `suchThat` (>= 0)
  return (Prop name)

--   Generates a random Form. The complexity / depth of this Form is defined by
-- size (a quickCheck parameter). 
genForm :: Gen Form 
genForm = sized formGenerator
  where
    formGenerator 0 = do
      n <- arbitrary
      return (Prop n)
      formGenerator n = oneof
      [ do
            f <- formGenerator (n - 1)
            return (Neg f)
      , do
            m <- choose (1, n - 1)
            f1 <- formGenerator m
            f2 <- formGenerator (n - m)
            return (Cnj [f1, f2])
      , do
            m <- choose (1, n - 1)
            f1 <- formGenerator m
            f2 <- formGenerator (n - m)
            return (Dsj [f1, f2])
      , do
            m <- choose (1, n - 1)
            f1 <- formGenerator m
            f2 <- formGenerator (n - m)
            return (Impl f1 f2)
      , do
            m <- choose (1, n - 1)
            f1 <- formGenerator m
            f2 <- formGenerator (n - m)
            return (Equiv f1 f2)
      ]
