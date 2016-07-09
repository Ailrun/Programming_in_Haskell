module Chapter07.Practices where

import Prelude hiding
  (
    
    all, any, takeWhile, dropWhile,
    map, filter
  )

import Data.Char

{- Practice 1 -}

{-
[f x | x <- xs, p x]
map f . filter p
-}

{- Practice 2 -}

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) | p x       = all p xs
             | otherwise = False

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) | p x       = True
             | otherwise = any p xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x       = x : takeWhile p xs
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) | p x       = dropWhile p xs
                   | otherwise = xs

{- Practice 3 -}

map :: (a -> b) -> [a] -> [b]
map f = foldr (\a bs -> f a : bs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x:xs else xs) []

{- Practice 4 -}

dec2int :: [Int] -> Int
dec2int = foldl (\v d -> v * 10 + d) 0

{- Practice 5 -}

{-
Those elements in list have different types.
-}

{- Practice 6 -}

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

{- Practice 7 -}

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int
type Bits = [Bit]

int2bin :: Int -> Bits
int2bin = unfold (==0) (`mod` 2) (`div` 2)

chop8 :: Bits -> [Bits]
chop8 = unfold null (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) (\(_:xs) -> xs)

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (\_ -> True) id f

{- Practice 8 -}

bin2int :: Bits -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

make8 :: Bits -> Bits
make8 bits = take 8 (bits ++ repeat 0)

chop9 :: Bits -> [Bits]
chop9 [] = []
chop9 x = take 9 x : chop9 (drop 9 x)

addparity :: Bits -> Bits
addparity bs = parity : bs
  where parity = sum bs `mod` 2

removeparity :: Bits -> Bits
removeparity (b:bs) | b == parity = bs
                    | otherwise   = error "Wrong Parity"
  where parity = sum bs `mod` 2

encode :: String -> Bits
encode = concat . map (addparity . make8 . int2bin . ord)

decode :: Bits -> String
decode = map (chr . bin2int . removeparity) . chop9

transmit :: String -> String
transmit = decode . channel . encode

{- Practice 9 -}

channel :: Bits -> Bits
channel = tail
