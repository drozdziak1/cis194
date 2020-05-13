module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

toDigitsRev :: Integer -> [Integer]
toDigitsRev n =
  case n of
    leZero
      | leZero <= 0 -> []
    _ -> (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [(* 2), id])

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

validate :: Integer -> Bool
validate n =
  let remainder = flip mod 10 $ sumDigits $ doubleEveryOther $ toDigits n
   in remainder == 0

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b tmp =
  case n of
    1 -> [(a, b)]
    n
      | n > 1 -> hanoi (n - 1) a tmp b ++ [(a, b)] ++ hanoi (n - 1) tmp b a
    _ -> []
