{-# LANGUAGE ForeignFunctionInterface #-}
--module Main where
module Chapter09.Examples where

import System.IO
--import GHC.IO.Encoding
--import System.Win32.Console
import Control.Concurrent

import Chapter08.Practices

{-
Linux Version

getCh :: IO Char
getCh = do e <- hGetEcho stdin
           b <- hGetBuffering stdin
           hSetEcho stdin False
           hSetBuffering stdin NoBuffering
           c <- getChar
           hSetEcho stdin e
           hSetBuffering stdin b
           return c
-}
{-
Windows Version
-}
import Control.Monad
import Data.Char
import Foreign.C

getCh :: IO Char
getCh = liftM (chr . fromEnum) c_getch
foreign import ccall unsafe "conio.h getch" c_getch :: IO CInt

{- Section 9.5 -}

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

{- Section 9.6 -}

{- Parsing -}

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
  where standard = "qcd=123+456-789*0()/"
        extra = "QCD \ESC\BS\DEL\n\r"

showbox :: IO ()
showbox = sequence_ [writeat (1, y) xs | (y, xs) <- zip [1..13] box]

display :: String -> IO ()
display xs = do writeat (3,2) $ "             "
                writeat (3,2) $ reverse . take 13 . reverse $ xs

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if c `elem` buttons
               then process c xs
               else do beep
                       calc xs

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC"    = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\r\n"     = eval xs
  | elem c "cC"        = clear
  | otherwise          = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
            [(n, "")] -> calc . show $ n
            _         -> do beep
                            calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

{-
{- Main for calculator -}

{- Since stdout has buffer by default, calculator needs to set NoBuffering-}
main :: IO()
main = do hSetBuffering stdout NoBuffering
          run
-}

{- Section 9.7 -}

width :: Int
width = 120
height :: Int
height = 30

type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

multiglider :: Board
multiglider = (map (\v -> \(x,y) -> (x+v,y+(v `div` 2))) (map (*5) [0..10])) <*> glider

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b
isEmpty :: Board -> Pos -> Bool
isEmpty b p = not . isAlive b $ p

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x  ,y-1),(x+1,y-1),
                          (x-1,y  ),          (x+1,y  ),
                          (x-1,y+1),(x  ,y+1),(x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width)  + 1,
              ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, (liveneighbs b p) `elem` [2,3]]

births :: Board -> [Pos]
births b = [p |
             p <- rmdups (concat (map neighbs b)),
             isEmpty b p,
             liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups = foldl (\acc x -> if x `elem` acc then acc else x:acc) []

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            threadDelay (50 * 1000)
            life (nextgen b)
{-
{- Main function for life game -}
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          life multiglider
-}
