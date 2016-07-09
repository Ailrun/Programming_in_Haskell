module Chapter11.Practices where

import Chapter11.Examples hiding
  (
    choices,


    valid
  )

{- Practice 1 -}

choices :: [a] -> [[a]]
choices ns = []

{- Practice 2 -}

removeFrom :: Eq a => a -> [a] -> [a]
removeFrom _ []                 = []
removeFrom v (x:xs) | v == x    = xs
                    | otherwise = x:removeFrom v xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice []     _       = True
isChoice (x:xs) origins = isChoice xs (removeFrom x origins)

{- Practice 3 -}

{-
The function is slower than original one, since it calculate useless spliting.
-}

{- Practice 4 -}

{-
length [e
       | ns <- chocies [1,3,7,19,25,50],
         e <- exprs ns]
-}

{-
length [e
       | ns <- chocies [1,3,7,19,25,50],
         e <- exprs ns,
         null . eval $ e]
-}

{- Practice 5 -}

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

{- And redefine every function using valid, then repeat practice 4. -}

{- Practice 6 -}
