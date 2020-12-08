module Day3Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

nextX :: Int -> Int -> Int -> Int
nextX x xShift len = (x + xShift) `mod` len

pathCreate :: Int -> Int -> [String] -> [Char]
pathCreate xloc xShift (a:as) = let len = length a in ((a !! xloc) : (pathCreate (nextX xloc xshift len) as)
pathCreate _    _      []     = []

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . length . filter (== '#') . pathCreate 0 3 . lines
    >> exitSuccess
