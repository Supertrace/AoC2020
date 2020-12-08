module Day2Prob1 where

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

validity :: String -> String -> Bool
validity field val = case field of 
                 "byr" -> (1920 <= v) && (2002 >= v) where v = (read val)::Int 
                 "iyr" -> (2010 <= v) && (2020 >= v) where v = (read val)::Int
                 "eyr" -> (2020 <= v) && (2030 >= v) where v = (read val)::Int
                 "hgt" -> let (a,b) = (span (`elem` ['0'..'9']) val) in case b of
                   "cm" -> (150 <= v) && (193 >= v) where v = (read a)::Int
                   "in" -> (59 <= v) && (76 >= v) where v = (read a)::Int
                   _    -> False
                 "hcl" -> (h == "#") && (length nums == 6) && (and checkNums) where 
                   (h,nums) = splitAt 1 val 
                   checkNums = map (`elem` (['0'..'9']++['a'..'f'])) nums
                 "ecl" -> val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                 "pid" -> and (map (`elem` ['0'..'9']) val) && (length val == 9)
                 "cid" -> True
                 _     -> False
                                                                
takeValidated :: [[(String,String)]] -> [[String]]
takeValidated lst = let valList = map (map fieldValidate) lst in 
                       map (map (\(a,b) -> a) . filter (\(a,b) -> b)) valList

checkFields :: [String] -> [String] -> Bool
checkFields toCheck appearing = ((filter (not . (flip elem appearing)) toCheck) == [])

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . length . filter id . map (checkFields fields) . takeValidated . grouper . map words . lines
    >> exitSuccess
--