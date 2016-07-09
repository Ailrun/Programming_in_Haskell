module Chapter06.Examples where

import Prelude hiding
  (
    
    (*),
    product, length, reverse, (++),
    zip, drop,

    even, odd,
    init
  )

{- Section 6.1 -}

factorial1 :: Int -> Int
factorial1 n = product [1..n]

factorial2 :: Int -> Int
factorial2 0 = 1
factorial2 n = n * factorial2 (n-1)

(*) :: Int -> Int -> Int
_ * 0 = 0
m * n = m + (m * (n - 1))

{- Section 6.2 -}

product :: Num a => [a] -> a
product [] = 1
product (n:ns) = n * product ns

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y:insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

{- Section 6.3 -}

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y):zip xs ys

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n [] = []
drop n (_:xs) = drop (n-1) xs

{- Section 6.4 -}

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

{- Section 6.5 -}

even :: Int -> Bool
even 0 = True
even n = odd (n-1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n-1)

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x:odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

{- Section 6.6 -}

init :: [a] -> [a]
init [_] = []
init (x:xs) = x : init xs

