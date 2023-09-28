import Data.List
import LTS
import Test.QuickCheck
import Data.Char

-- genIOLTS :: Gen ([State], [Label], [Label], [LabeledTransition], State)
-- genIOLTS = do
--     n <- choose (0, 10)
--     q <- [0..n]
--     li <- elements ['a'..'z']
--     lu <- elements ['a'..'z']
--     -- li <- genLabel
--     -- lu <- genLabel
--     q0 <- elements q
--     t <- genTransitions  q li lu s q0 10
--   --q0 <- (shuffle q) !! 0
--     return (q, [li], [lu], t, q0)


genTransition :: [State] -> [Label] -> [State] -> State -> Int -> Gen ([(State, Label, State)])
-- genTransition q li lu s n = do
--     q_new <- elements q
--     if not (even n) then
--         let reaction = elements li
--     else
--         let reaction = elements lu 
--     if n == 0 then
--         return ([(s, reaction, q_new)])
--     else
--         return genTransition q li lu q_new (n - 1) ++ ([(s, reaction, q_new)])

genTransition q li lu s n =
    do 
        if n == 0 then
            return ([(s, reaction, q_new)])
        else
            return (genTransition q li lu q_new (n - 1) ++ ([(s, reaction, q_new)]))
    where 
        if not (even n) then
            let reaction = elements li
        else
            reaction = elements lu  