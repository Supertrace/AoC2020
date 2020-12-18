module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List

parse :: String -> Int -> Int
parse [a] soFar      = read [a] + soFar
parse (a:rest) soFar = case a `elem` ['0'..'9'] of
  True -> case drop 1 rest of
            ('+':remain) -> parse (drop 1 remain) $ soFar + (read [a]) 
            ('*':remain) -> (soFar + read [a]) * (parse (drop 1 remain) 0)
  _    -> result where
            (expr, left) = parenPicker 1 rest
            toEval       = reverse $ drop 1 $ reverse expr
            answer       = soFar + parse toEval 0
            result       = case drop 1 left of
              ('+':remain) -> parse (drop 1 remain) $ answer
              ('*':remain) -> answer * (parse (drop 1 remain) 0)
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
    (readFile $ head args) >>= putStrLn . show . sum . map (flip parse 0) . map reverse . lines   
    >> exitSuccess
--