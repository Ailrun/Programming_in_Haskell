module Chapter10.Examples where

import Prelude hiding
  (
    
    Maybe(..)
  )

{- Section 10.1 -}

type Board = [Pos]
type Pos = (Int, Int)

{- Wrong declaration -}
--type Tree = (Int,[Tree])

type Parser' a = String -> [(a,String)]
-- type IO a = World -> (a, World)

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

{- Section 10.2 -}

--data Bool = False | True

data Move = MLeft | MRight | MUp | MDown

move :: Move -> Pos -> Pos
move MLeft  (x,y) = (x-1,y  )
move MRight (x,y) = (x+1,y  )
move MUp    (x,y) = (x  ,y-1)
move MDown  (x,y) = (x  ,y+1)

moves :: [Move] -> Pos -> Pos
moves ms p = foldl (\acc m -> move m acc) p ms

flip :: Move -> Move
flip MLeft  = MRight
flip MRight = MLeft
flip MUp    = MDown
flip MDown  = MUp

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^(2::Int)
area (Rect x y) = x * y

data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x

{- Section 10.3 -}

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

add' :: Nat -> Nat -> Nat
add' Zero     = id
add' (Succ m) = Succ . add' m

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree = Leaf Int | Node Tree Int Tree

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node lt n rt) = m == n || occurs m lt || occurs m rt

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node lt n rt) = flatten lt ++ [n] ++ flatten rt

stoccurs :: Int -> Tree -> Bool
stoccurs m (Leaf n) = m == n
stoccurs m (Node lt n rt) | m == n    = True
                          | m < n     = stoccurs m lt
                          | otherwise = stoccurs m rt

--data Tree a = Leaf a | Node (Tree a) (Tree a)
--data Tree a = Leaf | Node (Tree a) a (Tree a)
--data Tree a b = Leaf a | Node (Tree a b) b (Tree a b)
--data Tree a = Node a [Tree a]

{- Section 10.4 -}

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'B'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

evalP :: Subst -> Prop -> Bool
evalP _ (Const b) = b
evalP s (Var x) = find x s
evalP s (Not p) = not . evalP s $ p
evalP s (And p q) = evalP s p && evalP s q
evalP s (Imply p q) = evalP s p <= evalP s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups = foldl (\acc x -> if x `elem` acc then acc else x:acc ) []

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [evalP s p | s <- substs p]

{- Section 10.5 -}

data Expr = Val Int
          | Add Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y

type Cont = [Op]
data Op = EVAL Expr | ADD Int

eval' :: Expr -> Cont -> Int
eval' (Val n) c = exec' c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec' :: Cont -> Int -> Int
exec' [] n = n
exec' (EVAL y : c) n = eval' y (ADD n : c)
exec' (ADD n : c) m = exec' c (n + m)

value' :: Expr -> Int
value' e = eval' e []
