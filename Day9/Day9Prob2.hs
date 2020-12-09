module Day9Prob2 where

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

sumChecker :: Int -> [Int] -> [Int] -> Maybe [Int]
sumChecker num soFar (x:lst)
  | num == 0 && length soFar > 1 = Just soFar
  | num < 0                      = Nothing
  | True                         = sumChecker (num - x) (x:soFar) lst

sumHelp :: [Int] -> Int
sumHelp lst = let num = numFinder lst in sumMinMax $ sumFinder num lst 

sumFinder :: Int -> [Int] -> [Int]
sumFinder num lst@(_:rest) = case sumChecker num [] lst of
  Just a  -> a
  Nothing -> sumFinder num rest

sumMinMax :: [Int] -> Int
sumMinMax lst = m + bigM where
  m    = foldr1 min lst
  bigM = foldr1 max lst

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . sumHelp . map read . lines   
    >> exitSuccess
--