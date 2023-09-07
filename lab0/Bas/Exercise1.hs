import Test.QuickCheck

main :: IO ()
main = do
    let result = powerFun 10
    print result

addPowers :: Int -> Int
addPowers 0 = 0
addPowers x = (addPowers (x - 1)) + (x^2)

powerFun :: Int -> Int
powerFun x = div ((x * (x + 1)) * ((2 * x) + 1)) 6

propPowerFun :: Int -> Property
propPowerFun x = x >= 0 ==> addPowers x == powerFun x

addPowersThird :: Int -> Int
addPowersThird 0 = 0
addPowersThird x = (addPowersThird (x - 1)) + (x^3)

powerThirdFun :: Int -> Int
powerThirdFun x = (div (x * (x + 1)) 2) ^ 2 

propPowerThirdFun :: Int -> Property
propPowerThirdFun x = x >= 0 ==> addPowersThird x == powerThirdFun x
