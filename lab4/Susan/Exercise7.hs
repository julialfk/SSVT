module Exercise7 where

-- Time Spent: 15 minutes.

-- DIT IS WAT CHATGPT ER OVER TE ZEGGEN HAD. WSS NIET HEEL HANDIG OM DIT 1-OP-1 IN TE LEVEREN,
-- MAAR GEEFT WEL EEN GOED BEELD OVER WAT HET ANTWOORD ONGEVEER MOET ZIJN
-- MISSCHIEN DIT ANTWOORD COMBINEREN MET WAT JULIA AL ALS TEGENVOORBEELD HAD

-- The symmetric closure and the transitive closure operations do not commute, which means that there is a difference between the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R. Let me explain the difference and provide an example to illustrate it.

-- Symmetric Closure of the Transitive Closure:

-- If you first take the transitive closure of a relation R and then take the symmetric closure of the result, you are ensuring that if (a, b) is in the relation, then (b, a) will also be included in the relation. This operation doesn't necessarily guarantee that the relation is transitive.
-- Transitive Closure of the Symmetric Closure:

-- If you first take the symmetric closure of a relation R and then take the transitive closure of the result, you are ensuring that if (a, b) is in the relation, and (b, a) is also in the relation, then you add the transitive closure of these pairs. This operation doesn't necessarily guarantee that the relation is symmetric.
-- Here's an example to illustrate the difference:

-- Let R = {(1, 2), (2, 3)}. This is a non-transitive and non-symmetric relation.

-- Transitive Closure of R:

-- The transitive closure of R would include (1, 3) because we can reach 3 from 1 through 2. However, it doesn't include (2, 1) or (3, 2) as those pairs are not originally in R.
-- Transitive Closure of R = {(1, 2), (2, 3), (1, 3)}

-- Symmetric Closure of R:

-- The symmetric closure of R adds the reverse pairs for each element in R: (2, 1) and (3, 2).
-- Symmetric Closure of R = {(1, 2), (2, 3), (1, 3), (2, 1), (3, 2)}

-- Now, let's take the transitive closure of the symmetric closure from step 2:

-- For instance, (1, 2) and (2, 1) are in the symmetric closure, and their transitive closure includes (1, 1), (2, 2), and (1, 2), (2, 1).
-- Transitive Closure of Symmetric Closure of R = {(1, 2), (2, 3), (1, 3), (2, 1), (3, 2), (1, 1), (2, 2), (1, 2), (2, 1)}

-- Now, let's take the symmetric closure of the transitive closure from step 1:

-- We already obtained this result in step 1, which is {(1, 2), (2, 3), (1, 3)}.
-- As you can see, the results of these two operations are different. Therefore, the order of applying the operations matters, and the two are not the same.