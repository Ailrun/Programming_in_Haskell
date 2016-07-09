module Chapter08.Examples where

import Control.Monad
import Data.Char

{- Secion 8.2 -}

newtype Parser a = P {parse::(String -> [(a, String)])}

{- Section 8.3, 8.4 -}

instance Functor Parser where
  fmap f = (<*>) (pure f)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  p >>= f = P (\inp ->
                  case parse p inp of
                    [] -> []
                    ((v,out):_) -> parse (f v) out)
  fail _ = P (\_ -> [])

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)])

{- Section 8.5 -}

{- Consider about Arrow typeclass -}
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
                [] -> parse q inp
                rs -> rs)

{- Section 8.6 -}

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else fail ""

digit :: Parser Char
digit = sat isDigit
lower :: Parser Char
lower = sat isLower
upper :: Parser Char
upper = sat isUpper
letter :: Parser Char
letter = sat isAlpha
alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do _ <- char x
                   _ <- string xs
                   return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

ident :: Parser String
ident = do x <-lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

space :: Parser ()
space = do _ <- many (sat isSpace)
           return ()

{- Section 8.7 -}

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

list :: Parser [Int]
list = do _ <- symbol "["
          n <- natural
          ns <- many (do _ <- symbol ","
                         natural)
          _ <- symbol "]"
          return (n:ns)

{- Section 8.8 -}

expr :: Parser Int
expr = do t <- term
          ((do _ <- symbol "+"
               e <- expr
               return (t + e))
            +++
            return t)

term :: Parser Int
term = do f <- factor
          ((do _ <- symbol "*"
               t <- term
               return (f * t))
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
