module Chapter02.Practices where

{- Practice 1 -}

{-
2^3*4 == (2^3)*4
2*3+4*5 == (2*3)+(4*5)
2+3*4^5 == 2+(3*(4^5))
-}


{- Practice 2 -}

{-
SKIPPED
-}

{- Practice 3 -}

n = a `div` length xs
  where a = 10
        xs = [1,2,3,4,5]

{- Practice 4 -}

last1 xs = head (reverse xs)
last2 xs = xs!!(length xs - 1)

{- Practice 5 -}

init1 xs = reverse (tail (reverse xs))
init2 xs = take (length xs - 1) xs
