module Chapter12.Examples where

{- Section 12.5 -}

ones :: [Int]
ones = 1:ones

{- Section 12.6 -}

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p:sieve [x | x <- xs, x `mod` p /= 0]

{- Section 12.7 -}

sumwith :: Int -> [Int] -> Int
sumwith v [] = v
sumwith v (x:xs) = (sumwith $! (v+x)) xs

