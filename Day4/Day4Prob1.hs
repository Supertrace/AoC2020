module Day4Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

grouper :: [[String]] -> [[(String,String)]]
grouper []  = []
grouper lns = (concat (map (map $ splitter) group)) : (grouper rem) where 
                lsts = span (/= []) lns
                group = fst lsts
                rem = drop 1 $ snd lsts
                splitter lst = let (a,b) = span (/= ':') lst in (a, drop 1 b)

fieldValidate :: (String, String) -> (String, Bool)
fieldValidate (field, val) = (field, validity field val)
                                                                
takeValidated :: [[(String,String)]] -> [[String]]
takeValidated lst = let valList = map (map fieldValidate) lst in 
                       map (map (\(a,b) -> a) . filter (\(a,b) -> b)) valList

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . length . takeValidated . grouper . map words . lines
    >> exitSuccess
--