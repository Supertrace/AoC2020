module Day10Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

sorter :: [String] -> [Int]
sorter lst = 0 : (sort $ map read lst)

differences :: [Int] -> [Int]
differences lst = map (\(a,b) -> b - a) $ zip lst $ drop 1 lst

counter :: [Int] -> Int
counter lst = oneC * (threeC + 1) where -- plus 1 accounts for the device
                oneC   = length $ filter (== 1) lst
                threeC = length $ filter (== 3) lst

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . counter . differences . sorter . lines
    >> exitSuccess
--  