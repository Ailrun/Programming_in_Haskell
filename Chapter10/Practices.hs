module Chapter10.Practices where

import Chapter10.Examples hiding
  (

    stoccurs,

    Prop(..), p1, p2, p3, p4, evalP, vars, substs, isTaut,


    Expr(..), value, Cont, Op(..), exec', eval', value'
  )

{- Practice 1 -}

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)

{- Practice 2 -}

stoccurs :: Int -> Tree -> Bool
stoccurs n (Leaf m) = n == m
stoccurs n (Node lt m rt) = case compare n m of
                              EQ -> True
                              LT -> stoccurs n lt
                              GT -> stoccurs n rt

{- Practice 3 -}

data BTree = BLeaf Int | BNode BTree BTree

balancedPair :: BTree -> (Int, Bool)
balancedPair (BLeaf _) = (1, True)
balancedPair (BNode lt rt) = (ltn+rtn, ltb && rtb && (ltn - rtn) `elem` [-1,0,1])
  where (ltn, ltb) = balancedPair lt
        (rtn, rtb) = balancedPair rt

{- Practice 4 -}

halve :: [a] -> ([a], [a])
halve ls = (take hlen ls, drop hlen ls)
  where hlen = length ls `div` 2

balance :: [Int] -> BTree
balance [] = error "Empty Tree"
balance (x:[]) = BLeaf x
balance xs = BNode (balance lxs) (balance rxs)
  where (lxs, rxs) = halve xs

{- Practice 5 -}

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop

p1 :: Prop
p1 = Equiv (Not (And (Var 'A') (Var 'B'))) (Or (Not (Var 'A')) (Not (Var 'B')))
p2 :: Prop
p2 = Or (Not (Var 'A')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (Imply (Imply (Not (Var 'B')) (Not (Var 'A'))) (Var 'B'))

evalP :: Subst -> Prop -> Bool
evalP _ (Const b)   = b
evalP s (Var x)     = find x s
evalP s (Not p)     = not . evalP s $ p
evalP s (And p q)   = evalP s p && evalP s q
evalP s (Or p q)    = evalP s p || evalP s q
evalP s (Imply p q) = evalP s p <= evalP s q
evalP s (Equiv p q) = evalP s p == evalP s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [evalP s p | s <- substs p]

{- Practice 6 -}

{- Practice 7 -}

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
value (Mul x y) = value x * value y

type Cont = [Op]
data Op = AEVAL Expr | MEVAL Expr | ADD Int | MUL Int

eval' :: Expr -> Cont -> Int
eval' (Val n) c = exec' c n
eval' (Add x y) c = eval' x (AEVAL y : c)
eval' (Mul x y) c = eval' x (MEVAL y : c)

exec' :: Cont -> Int -> Int
exec' [] n = n
exec' (AEVAL y : c) n = eval' y (ADD n : c)
exec' (MEVAL y : c) n = eval' y (MUL n : c)
exec' (ADD n : c) m = exec' c (n + m)
exec' (MUL n : c) m = exec' c (n * m)

value' :: Expr -> Int
value' e = eval' e []

{- Practice 8 -}

{-
instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  Just x  >>= f = f x
-}

{-
instance Monad [] where
  return = (:[])
  []     >>= f = []
  (x:xs) >>= f = f x ++ (xs >>= f)
-}

