import Lecture3

cnf::Form -> Form
-- Remove double negative and implications/equivalences
cnf = associative . deMorgan . nnf . arrowfree

-- simplifycnf::Form -> Form
-- deMorgan law
deMorgan (Neg (Cnj [x, y])) = (Dsj [cnf (Neg x), cnf (Neg y)])
deMorgan (Neg (Dsj [x, y])) = (Cnj [cnf (Neg x), cnf (Neg y)])
deMorgan x = x
-- Associative law
associative (Cnj xs) = Cnj (loopThroughCnj xs)
associative (Dsj [x, Dsj [y,z]]) = cnf (Dsj[x,y,z])
associative x = x
-- Contradiction law
contradiction (Cnj x) = findContra x
contradiction (Dsj x) = findContra x
contradiction x = x

-- Loop thorugh a conjunction, add the elements of a conjunction
-- inside the conjuction together.
loopThroughCnj :: [Form] -> [Form]
loopThroughCnj [] = []
loopThroughCnj (x:xs) = 
    case x of
        Cnj y -> y ++ loopThroughCnj xs
        otherwise -> [x] ++ loopThroughCnj xs

-- Loop thorugh a disjunction, add the elements of a disjunction
-- inside the disjunction together.
loopThroughDsj :: [Form] -> [Form]
loopThroughDsj [] = []
loopThroughDsj (x:xs) = 
    case x of
        Dsj y -> y ++ loopThroughDsj xs
        otherwise -> [x] ++ loopThroughDsj xs

findContra :: [Form] -> [Form]
findContra [] = []
findContra (x:xs) = 
    case x of 
        Cnj ls ->
            case findContra ls of 
                True -> findContra xs
                False -> [x] ++ findContra xs
        Dsj ls ->
            case findContra ls of 
                True -> findContra xs
                False -> [x] ++ findContra xs
        otherwise -> [x] ++ findContra xs

-- Loop through a conjunction, try to find contradictions
isContra :: [Form] -> Bool
isContra [] = False
isContra (x:xs) = 
    case x of
        Prop y -> case (elem (Neg ( y)) xs) of
            True -> True
            False -> findContra xs
        otherwise -> findContra xs


-- (Equiv (Impl (Prop 1) (Prop 2)) (Impl (Neg (Prop 2)) (Neg (Prop 1))))




-- cnf (Neg (Neg (x))) = cnf x
-- cnf (Neg (Cnj [x, y])) = (Dsj [cnf (Neg x), cnf (Neg y)])
-- cnf (Neg (Dsj [x, y])) = (Cnj [cnf (Neg x), cnf (Neg y)])
-- cnf (Impl x y) = (Dsj [cnf (Neg x), cnf y])
-- cnf (Equiv x y) = (Cnj [cnf (Dsj [cnf (Neg (cnf x)), cnf y ]), (Dsj [cnf x, cnf (Neg (cnf y))])])
-- Associative law