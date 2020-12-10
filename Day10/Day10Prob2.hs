module Day6Prob2 where

import System.Environment
import System.Exit
import System.IO
import Data.List

sorter :: [String] -> [Int]
sorter lst = reverse $ (0 : (sort $ map read lst))

reachable :: [Int] -> Int
reachable (a:rest) = length $ takeWhile (<= a-3) rest

chainCount :: [(Int, Int)] -> [Int] -> [(Int,Int)]
chainCount kVals []           = kVals
chainCount kVals lst@(a:rest) = chainCount newVal rest where
  aVal   = case kVals of
             [] -> 1
             _  -> sum $ map snd $ filter ((<= a + 3) . fst) kVals
  newVal = (a, aVal) : kVals

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . snd . head .  chainCount [] . sorter . lines
    >> exitSuccess
--  