import Data.List
import System.Random
import Test.QuickCheck

-- Time spent: 4 hours.

data Boy =  Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

main :: IO ()
main = do
    -- print the name of each boy and the list of boys they accuse
    mapM_ printAccusations boys

    -- print who is guilty
    putStrLn $ "guilty: " ++ show guilty

    -- print who spoke the truth
    putStrLn $ "honest: " ++ show honest

-- boy1 accuses boy2 of having done it
-- Using pattern matching and guards, we can write down the statements of the boys
-- in logical formulas.
accuses :: Boy -> Boy -> Bool

-- "It was Matthew or it was Jack." -> It is true with Matthew and Jack, otherwise false.
accuses Peter boy2      | boy2 == Matthew || boy2 == Jack = True
                        | otherwise = False

-- "Carl didn't do it, and neither did I" -> so it is false with their names, and otherwise true.
accuses Matthew boy2    | boy2 == Carl || boy2 == Matthew = False
                        | otherwise = True

-- "Matthew and Peter are both lying" -> negate the statements of both Matthew and Peter (negate = not)
accuses Jack boy2       = not (accuses Matthew boy2) && not (accuses Peter boy2)

-- "Matthew or Peter is speaking the truth, but not both" -> literally translate this to logic using && and ||
accuses Arnold boy2     = (accuses Peter boy2 || accuses Matthew boy2) && not (accuses Peter boy2 && accuses Matthew boy2)

-- "What Arnold says is not true." -> negate his statement (by using not)
accuses Carl boy2       = not (accuses Arnold boy2)

-- generates a list of all the boys that accuse this boy
accusers :: Boy -> [Boy]
accusers boy = [accuser | accuser <- boys, accuses accuser boy]

-- Because exactly 3 boys speak the truth, and exactly 2 boys lie, we know that if someone is
-- accused exactly 3 times, they are guilty. So we search for the boy who has 3 accusers.
-- The boys speaking the truth are then the boys accusing this guilty boy.
guilty, honest :: [Boy]
guilty = [boy | boy <- boys, length (accusers boy) == 3]
honest = accusers (head guilty)

-- Jack is guilty, Matthew, Peter and Carl speak the truth.
printAccusations :: Boy -> IO ()
printAccusations boy1 = do
    putStrLn $ show boy1 ++ " is accused by: " ++ show (accusers boy1)
