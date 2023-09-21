data Boy = Matthew | Peter | Jack | Arnold | Carl
    deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False
accuses Jack x = (not (accuses Matthew x)) && (not (accuses Peter x))
accuses Arnold x = (((accuses Matthew x) || (accuses Peter x)) && (not((accuses Matthew x) && (accuses Peter x))))
accuses Carl x = (not (accuses Arnold x))

accusers :: Boy -> [Boy]
accusers name = [x | x <- boys, (accuses x name) ]

guilty, honest :: [Boy]
guilty = [x | x <- boys, (length (accusers x)) >= 3]

honest = [x | x <- boys, accuses x Jack]