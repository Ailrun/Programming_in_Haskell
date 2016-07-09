module Chapter04.Practices where

{- Practice 1 -}

{-
!!Warning!!
This function won't work with legal act
when it applies to list of odd elements.
-}
halve :: [a] -> ([a], [a])
halve x = (take l x, drop l x)
  where l = length x `div` 2

{- Practice 2 -}

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then []
               else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs

{- Practice 3 -}

(||) :: Bool -> Bool -> Bool
False || False = False
_     ||  _    = True

{- Practice 4 -}

(&&?) :: Bool -> Bool -> Bool
a &&? b = if a then
           if b then
             True
           else
             False
         else False

{- Practice 5 -}

(&&??) :: Bool -> Bool -> Bool
a &&?? b = if a then b
           else False

{- Practice 6 -}

mult :: Num a => a -> a -> a -> a
mult = \x -> (\y -> (\z -> x * y * z))
