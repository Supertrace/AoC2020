module Day6Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

grouper :: [String] -> [[String]]
grouper []  = []
grouper lns = (group) : (grouper rem) where 
                lsts  = break (== []) lns
                group = fst lsts
                rem   = drop 1 $ snd lsts

counter :: String -> [String] -> String
counter result []         = result
counter result (str:strs) = let newRes = [a | a <- str, not (a `elem` result)] ++ result in counter newRes strs

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . sum . map length . map (counter []) . grouper . lines   
    >> exitSuccess
--