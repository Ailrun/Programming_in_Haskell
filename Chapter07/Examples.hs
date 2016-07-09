module Chapter07.Examples where

import Prelude hiding
  (
    
    map, filter,
    foldr, length, reverse, (++),
    sum, product, or, and, foldl,
    odd
  )

import Data.Char

{- Section 7.1 -}

add :: Int -> Int -> Int
add = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

{- Section 7.2 -}

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x:map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) | p x       = x:filter p xs
                | otherwise = filter p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^(2::Int)) (filter even ns))

{- Section 7.3 -}

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

length :: [a] -> Int
length = foldr (\_ n -> 1 + n) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse :: [a] -> [a]
reverse = foldr snoc []

(++) :: [a] -> [a] -> [a]
(++) = flip (foldr (:))

{- Section 7.4 -}

sum :: Num a => [a] -> a
sum = foldl (+) 0

product :: Num a => [a] -> a
product = foldl (*) 1

or :: [Bool] -> Bool
or = foldl (||) False

and :: [Bool] -> Bool
and = foldl (&&) True

length' :: [a] -> Int
length' = foldl (\n _ -> n + 1) 0

reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x:xs) []

(++?) :: [a] -> [a] -> [a]
(++?) = foldl (\xs y -> xs ++ [y])

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ v [] = v
foldl f v (x:xs) = foldl f (f v x) xs

{- Section 7.5 -}

odd :: Integral a => a -> Bool
odd = not . even

twice' :: (a -> a) -> a -> a
twice' f = f . f

sumsqreven' :: Integral a => [a] -> a
sumsqreven' = sum . map (^(2::Int)) . filter even

compose :: [a -> a] -> a -> a
compose = foldr (.) id

{- Section 7.6 -}

{- Subsection 7.6.2 -}

type Bit = Int
type Bits = [Bit]

bin2int :: Bits -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> Bits
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: Bits -> Bits
make8 bits = take 8 (bits ++ repeat 0)

{- Subsection 7.6.3 -}

encode :: String -> Bits
encode = concat . map (make8 . int2bin . ord)

chop8 :: Bits -> [Bits]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: Bits -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: Bits -> Bits
channel = id
