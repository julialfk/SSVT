isPermutation :: Eq a => [a] -> [a] ->  Bool
isPermutation (x:xs) ls = 
    case contains x ls of
        False -> False
        True -> isPermutation xs ls