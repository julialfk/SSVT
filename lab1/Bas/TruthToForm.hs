import Lecture3

truthTable :: Form -> [(Bool, Valuation)]
truthTable f = map (\x -> ((evl x f), x)) (allVals f)

getTrue :: [(Bool, Valuation)] -> [Valuation]
getTrue [] = []
getTrue ((truth, vals):xs) = 
    case truth of 
        False -> [vals] ++ getTrue xs
        True -> getTrue xs

finalForm :: [Form] -> Form
finalForm ls = Cnj ls

makeFormula :: [Valuation] -> [Form]
makeFormula [] = []
makeFormula (x:xs) = [(Dsj (getForm x))] ++ makeFormula xs

getForm :: [(Int, Bool)] -> [Form]
getForm [] = []
getForm ((propN, truth):xs) = 
    case truth of
        False -> [(Prop propN)] ++ getForm xs
        True -> [(Neg (Prop propN))] ++ getForm xs

getTrueForm :: Form -> Form
-- getTrueForm (Prop x) = Dsj((Prop x), (Neg (Prop x)))
-- getTrueForm (Dsj xs) = getTrueForm
getTrueForm form = Dsj [(Prop 1), (Neg (Prop 1))]

getCnf :: Form -> Form
getCnf form = do
    let table = truthTable form
    let allTrue = getTrue table
    if (length allTrue) == 0 then getTrueForm form else 
        let test = makeFormula allTrue
        finalForm test