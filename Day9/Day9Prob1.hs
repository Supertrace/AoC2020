module Day9Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

checker :: Int -> [Int] -> Bool
checker num lst = (/=) [] $ [x+y | x <- lst, y <- lst, x+y == num]

comparer :: [Int] -> (Int,Bool) 
comparer lst = (num, checker num a) where
  (a,b) = splitAt 25 lst
  num   = head b

lstCheck :: [Int] -> [(Int,Bool)]
lstCheck lst@(num:rest) = comparer lst : (lstCheck rest)
lstCheck [] = error "oops" 

numFinder :: [Int] -> Int
numFinder = fst . head . dropWhile snd . lstCheck

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . numFinder . map read . lines   
    >> exitSuccess
--