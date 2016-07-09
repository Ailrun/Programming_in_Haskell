module Chapter05.Practices where

import Data.Char

{- Practice 1 -}

sqrsum2hund :: (Num a, Enum a) => a
sqrsum2hund = sum [x^(2::Int) | x <- [1..100]]

{- Practice 2 -}

replicate :: Int -> a -> [a]
replicate n v = [v | _ <- [1..n]]

{- Practice 3 -}

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) |
           x <- [1..n], y <- [1..n], z <- [1..n],
           x^(2::Int) + y^(2::Int) == z^(2::Int)]

{- Practice 4 -}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects:: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == 2*x]

{- Practice 5 -}

{-
[(x, y) | x <- [1..n], y <- [1..n]]
concat [[(x,y) | x <- [1..n]] | x <- [1..n]]
-}

{- Practice 6 -}


find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [1..n])
  where n = length xs - 1

{- Practice 7 -}

scalaproduct :: [Int] -> [Int] -> Int
scalaproduct xs ys = sum [(x*y) | (x,y) <- zip xs ys]

{- Practice 8 -}

{- Use next function instead of original 'shift' -}

newshift :: Int -> Char -> Char
newshift n c | isLower c = charshift n c 'a' 
             | isUpper c = charshift n c 'A'
             | otherwise = c
  where charshift n' c' d = int2char ((char2int c' d + n') `mod` 26) d
        char2int c' d = ord c' - ord d
        int2char n' d = chr (n' + ord d)
