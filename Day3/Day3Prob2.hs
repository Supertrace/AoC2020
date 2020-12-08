module Day2Prob1 where
-- Searches through file to find two numbers summing to 2020
-- and then mutliplies them together
-- usage: Day1Prob1 filename

import System.Environment
import System.Exit
import System.IO
import Data.List

slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

nextX :: Int -> Int -> Int -> Int
nextX x xShift len = (x+xShift) `mod` len

pathCreate :: Int -> Int -> Int -> [String] -> [Char]
pathCreate xloc xShift yShift (a:as) = let len = length a in ((a !! xloc) : (pathCreate (nextX xloc xShift len) xShift yShift $ drop (yShift - 1) as))
pathCreate _    _      _      []     = [] 

apply :: [a -> b] -> a -> [b]
apply []     _     = []   
apply (f:fs) input = (f input) : apply fs input 

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . product . map (length . filter (== '#')) . apply (map (uncurry $ pathCreate 0) slopes) . lines
    >> exitSuccess
