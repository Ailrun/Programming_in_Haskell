module Chapter09.Practices where

import Prelude hiding (getLine)

import Chapter08.Practices
import Chapter09.Examples

{- Practice 1 -}

{-
Original (Prelude's) getLine allows delete action.
Futhermore, this getLine allows delete action too.
-}

getLine :: IO String
getLine = do x <- getChar
             if x == '\n'
               then return []
               else do xs <- getLine
                       return (x:xs)

{- Practice 2 -}

eval :: String -> IO ()
eval xs = case parse expr xs of
            [(n, "")] -> calc . show $ n
            _         -> do beep
                            calc xs
