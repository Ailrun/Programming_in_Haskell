module Chapter12.Practices where

{- Practice 1 -}

{-
2*3 is most inner and most outer
-}

{-
1+2 is most inner and most outer
-}

{-
fst is most outer
1+2 is most inner
-}

{-
2*3 is most inner and most outer
-}

{- Practice 2 -}

{-
When we use outermost evaluation, we need not evaluate 2+3.
-}

{- Practice 3 -}

{-
mult 3 4
{ apply mult }
(\y -> 3 * y) 4
{ apply \y -> 3 * y}
3 * 4
{ apply * }
12
-}

{- Practice 4 -}

fibs :: [Integer]
fibs = 0:1:[f0 + f1 | (f0, f1) <- zip fibs (tail fibs)]

fibs' :: [Integer]
fibs' = fibs'helper 0 1

fibs'helper :: Integer -> Integer -> [Integer]
fibs'helper a b = a:fibs'helper b (a+b)

{- Practice 5 -}

fib :: Int -> Integer
fib n = fibs !! n

{-
head [e
     | n <- [1..],
       fib n > 10000]
-}

{- Practice 6 -}

data Tree a = Leaf | Node (Tree a) a (Tree a)

repeatT :: a -> Tree a
repeatT x = xt
  where xt = Node xt x xt

takeT :: Int -> Tree a -> Tree a
takeT 0 _              = Leaf
takeT _ Leaf           = Leaf
takeT n (Node lt v rt) = Node (takeT (n-1) lt) v (takeT (n-1) rt)

replicateT :: Int -> a -> Tree a
replicateT n = takeT n . repeatT
