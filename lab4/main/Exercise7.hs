module Exercise7 where

-- Time Spent: 15 minutes.

-- Is there a difference between the symmetric closure of the transitive closure of a relation R
-- and the transitive closure of the symmetric closure of R ?

-- Yes, there is a difference. These two sets are not the same. See below for an explanation:

-- Symmetric Closure of the Transitive Closure:
-- First you take the transitive closure of R and then the symmetric closure of the result.
-- This ensures that if (a, b) is in the relation, then (b, a) will also be in there.
-- This doesn't necessarily mean that the relation is transitive.

-- Transitive Closure of the Symmetric Closure:
-- First you take the symmetric closure of R and then take the transitive closure of the result.
-- This ensures that if (a, b) is in the relation, and (b, a) is also in the relation, and then
-- we add the transitive closure of these pairs.
-- This doesn't necessarily mean that the relation is symmetric.

-- See the following counter example as a proof:
-- R = [(1,2),(2,3),(3,4)]

-- Transitive Closure of R:
    -- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]. We derived this from our function written in Exercise 5.

-- Symmetric Closure of R:
    -- [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]. We derived this from our function written in Exercise 3.

-- Symmetric Closure of the Transitive Closure:
    -- [(1,2),(1,3),(1,4),(2,1),(2,3),(2,4),(3,1),(3,2),(3,4),(4,1),(4,2),(4,3)]. We derived this from our function written in Exercise 3.

-- Transitive Closure of the Symmetric Closure:
    -- [(1,2),(1,1),(2,1),(2,3),(2,2),(3,2),(3,4),(3,3),(4,3)]. We derived this from our function written in Exercise 5.

-- As you can see, these two sets are not equal and therefore there is a difference between the symmetric
-- closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R.

