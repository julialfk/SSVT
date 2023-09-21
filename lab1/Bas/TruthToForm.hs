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
finalForm (x:ls) = 
    if (length (x:ls)) > 1 then Cnj (x:ls) else x

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
getTrueForm (Prop x) = Dsj[(Prop x), (Neg (Prop x))]
getTrueForm (Dsj (x:xs)) = getTrueForm x
getTrueForm (Cnj (x:xs)) = getTrueForm x
getTrueForm (Neg x) = getTrueForm x
getTrueForm (Impl x y) = getTrueForm x
getTrueForm (Equiv x y) = getTrueForm x
getTrueForm _ = Dsj[Prop 1, Prop 2]

getCnf :: Form -> Form
getCnf form = do
    let table = truthTable form
    let allTrue = getTrue table
    let formula = makeFormula allTrue
    if (length formula) == 0 then (getTrueForm form) else 
        (finalForm formula)