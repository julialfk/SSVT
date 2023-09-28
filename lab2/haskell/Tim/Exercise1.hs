module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

--type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)


--validateLTS :: IOLTS -> Bool
--validateLTS (Q, L, T, q0) = True
--validateLTS (Q, Li, Lu, T, q0) = True


--myFunction :: [State] -> [Label] -> [Label] -> [LabeledTransition] -> State -> Bool
--myFunction q l t transitions q0 =

--n arbitrary
--[0..n]

-- gen single letters

-- pick random number left from 0 to n, pick random letter, pick random number right

-- pick random start state


-- tretmanS4 = ([0,1,2,3],["a"],["x"],[(0,"a",1),(1,"x",2),(1,"tau",3)],0)

main :: IO ()
main = do
    print tretmanS4
    --print validateLTS tretmanK3