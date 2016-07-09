module Chapter03.Practices where

{- Practice 1 -}

{-
['a', 'b', 'c'] :: [Char]
('a', 'b', 'c') :: (Char, Char, Char)
[(False, '0'), (True, '1')] :: [(Bool, Char)]
[tail, init, reverse] :: [[a] -> [a]]
-}

{- Practice 2 -}

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq t => [t] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

{- Practice 3 -}

{-
SKIPPED
-}

{- Practice 4 -}

{-
Since function can have infinite number of cases of inputs, so it is impoassible to compare every case one by one.

In the case where only finite number of cases of inputs is possible, we can compare every result one by one, and we can compare those functions.
-}
