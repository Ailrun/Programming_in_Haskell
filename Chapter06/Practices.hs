module Chapter06.Practices where

import Prelude hiding
  (
    (^),

    and, concat, replicate, (!!), elem,


    sum, take, last
  )

{- Practice 1 -}
(^) :: Num a => a -> Int -> a
_ ^ 0 = 1
x ^ n = x * (x ^ (n  -1))


{- Practice 2 -}

{-
length [1,2,3]
{ apply length }
1 + length [2,3]
{ apply length }
1 + (1 + length [3])
{ apply length }
1 + (1 + (1 + length []))
{ apply length }
1 + (1 + (1 + 0))
{ apply (+) }
3
-}

{-
drop 3 [1,2,3,4,5]
{ apply drop }
drop 2 [2,3,4,5]
{ apply drop }
drop 1 [3,4,5]
{ apply drop }
drop 0 [4,5]
{ apply drop }
[4,5]
-}

{-
init [1,2,3]
{ apply init }
1:init [2,3]
{ apply init }
1:(2:init [3])
{ apply init }
1:(2:[])
{ list expression }
[1,2]
-}

{- Practice 3 -}

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

concat :: [[a]] -> [a]
concat [] = []
concat (l:ls) = l ++ concat ls

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n v = v:replicate (n-1) v

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (x':xs) | x == x'   = True
               | otherwise = elem x xs

{- Practice 4 -}

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x >= y    = y:merge (x:xs) ys
                    | otherwise = x:merge xs (y:ys)

{- Practice 5 -}

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs = merge (msort lxs) (msort rxs)
  where (lxs, rxs) = halve xs

{- Practice 6 -}

sum :: Num a => [a] -> a
sum [] = 0
sum (n:ns) = n + sum ns

take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x:xs) = x:take (n-1) xs

last :: [a] -> a
last (x:[]) = x
last (_:xs) = last xs
