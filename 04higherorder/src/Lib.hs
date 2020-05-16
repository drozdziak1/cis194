module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldr (\x acc -> acc * (x - 2)) 1 $ filter even xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' xs =
  sum $
  takeWhile (> 0) $
  iterate
    (\x ->
       if x == 1
         then 0
         else if even x
                then x `div` 2
                else 3 * x + 1)
    xs
