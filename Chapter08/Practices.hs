module Chapter08.Practices
  (
    module Chapter08.Examples,
    expr
  )
where

import Prelude hiding
  (
  )
import Chapter08.Examples hiding
  (
    expr, term, factor, eval
  )

{- Practice 1 -}

int :: Parser Int
int = ((do _ <- char '-'
           n <- natural
           return (-n))
       +++
       natural)

{- Practice 2 -}

comment :: Parser ()
comment = do _ <- string "--"
             _ <- many (sat (/= '\n'))
             ((do _ <- char '\n'
                  return ())
               +++
               return ())

{- Practice 3 -}

{-
2+3+4
            expr
         /    |   \
      expr    +   expr
     /  |  \        |
  expr  +  expr   term
    |        |      |
  term     term  factor
    |        |      |
 factor   factor    4
    |        |
    2        3
-}

{-
2+3+4
         expr
       /   |   \
   expr    +   expr
     |       /   |  \
   term   expr   + expr
     |      |        |
  factor  term     term
     |      |        |
     2   factor   factor
            |        |
            3        4
-}

{- Practice 4 -}

{-
2+3
     expr
    /  |  \
 term  +  expr
   |        |
factor    term
            |
         factor
-}

{-
2*3*4
        expr
          |
        term
     /    |    \
factor    *    term
   |          /  |  \       
   2     factor  *  term
            |         |
            3      factor
                      |
                      4
-}

{-
(2+3)+4
            expr
         /    |    \
      term    +    expr
        |            |
     factor        term
   /    |    \       |
  (   expr    )   factor
     /  |  \         |
  term  +  expr      4
    |        |
 factor    term
    |        |
    2     factor
             |
             3
-}

{- Practice 5 -}

{-
Since we must check operational expression first, we need to parse the number double times when we deal with simple number expression.
-}

{- Practice 6 -}

expr :: Parser Int
expr = do t <- term
          ((do _ <- symbol "+"
               e <- expr
               return (t + e))
            +++
            (do _ <- symbol "-"
                e <- expr
                return (t - e))
            +++
            return t)

term :: Parser Int
term = do f <- factor
          ((do _ <- symbol "*"
               t <- term
               return (f * t))
            +++
            (do _ <- symbol "/"
                t <- term
                return (div f t))
            +++
            return f)

factor :: Parser Int
factor = ((do _ <- symbol "("
              e <- expr
              _ <- symbol ")"
              return e)
          +++
          natural)

eval :: String -> Int
eval cs = case parse expr cs of
            ((n,[]):_)   -> n
            ((_,left):_) -> error ("There is none-expr input: " ++ left)
            []           -> error "Wrong input"

{- Practice 7 -}

{-
expr' := term' ( + expr' | - expr' | \epsilon )
term' := factor' ( * term' | / term' | \epsilon )
factor' := base' ( ^ factor' | \epsilon)
base' := (expr') | num
num := ... | -2 | -1 | 0 | 1 | 2 | ...
-}

expr' :: Parser Int
expr' = do t <- term'
           ((do _ <- symbol "+"
                e <- expr'
                return (t + e))
            +++
            (do _ <- symbol "-"
                e <- expr'
                return (t - e))
            +++
            return t)

term' :: Parser Int
term' = do f <- factor'
           ((do _ <- symbol "*"
                t <- term'
                return (f * t))
            +++
            (do _ <- symbol "/"
                t <- term'
                return (div f t))
            +++
            return f)

factor' :: Parser Int
factor' = do b <- base'
             ((do _ <- symbol "^"
                  f <- factor'
                  return (b ^ f))
               +++
               return b)

base' :: Parser Int
base' = ((do _ <- symbol "("
             e <- expr'
             _ <- symbol ")"
             return e)
         +++
         natural)

eval' :: String -> Int
eval' cs = case parse expr' cs of
            ((n,[]):_)   -> n
            ((_,left):_) -> error ("There is none-expr input: " ++ left)
            []           -> error "Wrong input"

{- Practice 8 -}

{- (a) -}
{-
expr'' := expr'' (- num | e)
num := ... | -2 | -1 | 0 | 1 | 2 | ...
-}

{- (b) -}
expr'' :: Parser Int
expr'' = do e <- expr''
            ((do _ <- symbol "-"
                 n <- natural
                 return (e - n))
              +++
              return e)

{- (c) -}
{-
This loops forever since parsing result of expr'' is not determined
-}

{- (d) -}
expr''' :: Parser Int
expr''' = do n <- natural
             mns <- many (do _ <- symbol "-"
                             mn <- natural
                             return mn)
             let res = foldl (\acc mn -> acc - mn) n mns
             return res
