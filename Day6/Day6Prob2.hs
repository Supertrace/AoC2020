module Day2Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

grouper :: [String] -> [[String]]
grouper []  = []
grouper lns = (group) : (grouper rem) where 
                lsts = span (/= []) lns
                group = fst lsts
                rem = drop 1 $ snd lsts

counter :: String -> [String] -> String
counter result [] = result
counter result (str:strs) = let newRes = filter (`elem` str) result in counter newRes strs

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . sum . map length . map (counter ['a'..'z']) . grouper . lines   
    >> exitSuccess
--