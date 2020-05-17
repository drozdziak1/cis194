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
fun2' n =
  sum $
  filter even $
  takeWhile (/= 1) $
  iterate
    (\x ->
       if even x
         then x `div` 2
         else 3 * x + 1)
    n

insertBalanced :: Tree a -> a -> Tree a
insertBalanced Leaf x = Node 0 Leaf x Leaf
insertBalanced (Node height left val right ) x =
      let newLeft = insertBalanced left x
          newRight = insertBalanced right x
       in case (tHeight left, tHeight right) of
               (lheight , rheight) | lheight > rheight ->
                Node (rheight + 1) left val newRight
               (lheight, rheight) ->
                 Node (lheight + 1) newLeft val right

-- tree height getter
tHeight :: Tree a -> Integer
tHeight Leaf = 0
tHeight (Node n _ _ _) = n

foldTree :: [a] -> Tree a
foldTree = foldr (flip insertBalanced) Leaf

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)


-- True if there's an odd number of True's in input
xor :: [Bool] -> Bool
xor = foldr (/=) False . filter id

-- fold-based map
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- foldr-based foldl. The task mentions a -> b -> a but I think that's not
-- right.
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr (flip f) z $ reverse xs

-- Sieve of Sundaram implementation
sieveSundaram :: Integer -> [Integer]
sieveSundaram  = undefined

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]
