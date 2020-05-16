module Lib where

everyNth :: [a] -> Int -> [a]
everyNth _ 0 = []
everyNth xs n =
  foldr
    (\(idx, el) acc ->
       if mod idx n == 0
         then el : acc
         else acc)
    [] $
  zip (enumFrom 1) xs

skips :: [a] -> [[a]]
skips xs = [everyNth xs i | i <- [1 .. length xs]]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) =
  if x < y && z < y
    then y : localMaxima (z : xs)
    else localMaxima (y : z : xs)
localMaxima _ = [] -- Too short

count :: [Integer] -> Integer -> Integer
count xs n = toInteger $ length $ filter (== n) xs

histogram :: [Integer] -> String
histogram xs =
  let counts = [count xs digit | digit <- [0 .. 9]]
      height = maximum counts
      lns =
        map
          (\curH ->
             map
               (\c ->
                  if c >= curH
                    then '*'
                    else ' ')
               counts) $
        reverse [1 .. height]
   in unlines lns ++ "==========\n0123456789"
