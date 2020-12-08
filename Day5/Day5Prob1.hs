module Day2Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

bin :: String -> [Int]
bin lst = let (a,b) = splitAt 7 lst in (map rowMap a) ++ (map colMap b)

rowMap :: Char -> Int
rowMap char = case char of
  'F' -> 0
  'B' -> 1
  _   -> error "oops"

colMap :: Char -> Int
colMap char = case char of
  'L' -> 0
  'R' -> 1
  _   -> error "oops"

deBin :: [Int] -> Int
deBin = sum . map (\(a,b) -> b * (2^(10-a))) . zip [1..]

missingFinder :: [Int] -> Int
missingFinder lst = case find (\(a,b) -> b-a == 2) $ zip lst $ drop 1 lst of
  Just (a,b) -> a+1
  Nothing    -> error "oops"

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . missingFinder . sort . map (deBin . bin) . lines   
    >> exitSuccess
--