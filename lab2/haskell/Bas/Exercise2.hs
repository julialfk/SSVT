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

genTransition :: [State] -> [Label] -> [Label] -> State -> Int -> Gen [(State, Label, State)]
genTransition q li lu s n = do
    q_new <- elements q
    let reaction = if even n then elements lu else elements li
    if n == 0
        then return [(s, reaction, q_new)]
        else do
            rest <- genTransition q li lu q_new (n - 1)
            return $ (s, reaction, q_new) : rest