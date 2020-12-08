module Day1Prob1 where
-- Searches through file to find two numbers summing to 2020
-- and then mutliplies them together
-- usage: Day1Prob1 filename

import System.Environment
import System.Exit
import System.IO
import Data.List

nummers :: String -> [Integer]
-- Turns file contents into list of integers
nummers = map read . filter (/= "") . lines

find3 :: [Integer] -> String
find3 xs = show $ find ((== 2020).fst) $ [(i + j + k, i * j * k) | i <- xs, j <- xs, k <- xs]

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStr. find3 . nummers >> exitSuccess
