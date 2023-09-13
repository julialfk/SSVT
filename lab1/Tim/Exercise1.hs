factorial :: Integer -> Integer

factorial 0 = 1
factorial 1 = 1
factorial p = p * factorial p-1


nPn :: Int -> Int
formulaCubes n = n^n

prop_NpowerN :: Int -> Property
prop_NpowerN n = n >= 0 ==> sumCubes n == formulaCubes n

n^n
n^2

# succesor altijd twee keer zo groot of groot 
# altijd kleiner dan n^n