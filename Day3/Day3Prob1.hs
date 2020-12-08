module Day2Prob1 where
-- Searches through file to find two numbers summing to 2020
-- and then mutliplies them together
-- usage: Day1Prob1 filename

import System.Environment
import System.Exit
import System.IO
import Data.List

nextX :: Int -> Int -> Int -> Int
nextX x xShift len = (x+xShift) `mod` len

pathCreate :: Int -> Int -> [String] -> [Char]
pathCreate xloc (a:b:as) = ((a !! xloc) : (pathCreate (nextX xloc xshift len) as)
pathCreate xloc [a]    = [a !! xloc] 
pathCreate _    []     = [] 

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . length . filter (== '#') . pathCreate 0 1 . lines
    >> exitSuccess
