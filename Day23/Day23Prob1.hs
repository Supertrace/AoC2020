module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import Data.Char

makeMove :: [Int] -> [Int]
makeMove (curr:a:b:c:rest) = newOrd where
  (bef,(top:aft)) = break (== (goal curr [a,b,c])) rest
  newOrd = bef ++ [top, a, b, c] ++ aft ++ [curr]

goal :: Int -> [Int] -> Int
goal a lst = let newA = (a-1) `mod` 9 in case newA `elem` lst of
           True -> goal newA lst
           _    -> newA

helper :: [Int] -> [Int]
helper input  = head $ drop 100 $ iterate makeMove input

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . map ((`mod` 9) . digitToInt) . head . lines   
    >> exitSuccess
--