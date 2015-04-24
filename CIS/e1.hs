toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits a = reverse (toDigitsRev a) 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (f:s:r) = f : (s*2) : doubleEveryOther r 
doubleEveryOther (f:r) = f : doubleEveryOther r 

sumDigits :: [Integer] -> Integer
sumDigits [] = 0 
sumDigits (f:r) = (foldl (+) 0 (toDigits f)) + sumDigits r

isValid :: Integer -> Bool
isValid x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0
