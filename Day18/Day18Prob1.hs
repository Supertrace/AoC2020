module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List

parse :: String -> Int
parse [a]      = read [a]
parse (a:rest) = case a `elem` ['0'..'9'] of
  True -> case drop 1 rest of
            ('+':remain) -> read [a] + (parse $ drop 1 remain)
            ('*':remain) -> read [a] * (parse $ drop 1 remain)
  _    -> result where
            (expr, left) = parenPicker 1 rest
            toEval       = reverse $ drop 1 $ reverse expr
            answer       = parse toEval
            result       = case drop 1 left of
              ('+':remain) -> answer + (parse $ drop 1 remain)
              ('*':remain) -> answer * (parse $ drop 1 remain)
              []           -> answer

parenPicker :: Int -> String -> (String,String)
parenPicker 0   rest     = ([], rest)
parenPicker num (a:rest) = ((a : next), str) where
  (next, str) = case a of 
    '(' -> parenPicker (num - 1) rest
    ')' -> parenPicker (num + 1) rest
    _   -> parenPicker num       rest
 

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . sum . map parse . map reverse . lines   
    >> exitSuccess
--