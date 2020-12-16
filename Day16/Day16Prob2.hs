module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import GHC.Exts

grouper :: [String] -> [[String]]
grouper []  = []
grouper lns = (group) : (grouper rem) where 
                lsts  = break (== []) lns
                group = fst lsts
                rem   = drop 1 $ snd lsts

ruleParse :: String -> (String, Int, Int, Int, Int)
ruleParse str = (field, min1, max1, min2, max2) where
  (field , vals')   = break (== ':') str
  vals         = drop 2 vals'
  (rng1, rng2) = break (== ' ') vals
  realRng2 = drop 4 rng2
  ((min1, max1), (min2, max2)) = (minMax rng1, minMax realRng2)

minMax :: String -> (Int, Int)
minMax str = (read a, read b) where
  (a,b') = break (== '-') str
  b      = drop 1 b'

ticketParse :: String -> [Int]
ticketParse []  = []
ticketParse str = read a : ticketParse b where
  (a,b') = break (== ',') str
  b      = drop 1 b'

inputP :: [[String]] -> ([(String,Int,Int,Int,Int)], [Int], [[Int]])
inputP [rule, [_,tik], (_:tix)] = (map ruleParse rule, ticketParse tik, map ticketParse tix)

ruleC :: (String,Int,Int,Int,Int) -> Int -> Bool
ruleC (_,min1,max1,min2,max2) val = (val >= min1 && val <= max1) || (val >= min2 && val <= max2)

isValid :: [(String,Int,Int,Int,Int)] -> Int -> Bool
isValid rules val = any (flip ruleC val) rules

checker :: [(String,Int,Int,Int,Int)] -> [[Int]] -> [[Int]]
checker rules vals = filter (all (isValid rules)) vals

fieldF :: [[(String,Int,Int,Int,Int)]] -> [[Int]] -> [[(String,Int,Int,Int,Int)]]
fieldF possFields []         = possFields
fieldF possFields (tik:rest) = fieldF newPos rest where
  zpped = zip possFields tik
  newPos = map (fieldWinnow) zpped
  fieldWinnow :: ([(String,Int,Int,Int,Int)], Int) -> [(String,Int,Int,Int,Int)]
  fieldWinnow ([rule], _)  = [rule]
  fieldWinnow (rules, val) = filter (flip ruleC val) rules

helper :: ([(String,Int,Int,Int,Int)], [Int], [[Int]]) -> ([[(String,Int,Int,Int,Int)]], [Int])
helper (rules, tik, vals@(val:_)) = (fieldF startPoss validVals, tik) where
  startPoss = map snd $ zip val $ iterate id rules
  validVals = checker rules vals

possRed :: [[(String,Int,Int,Int,Int)]] -> [(String,Int,Int,Int,Int)] 
possRed poss = case foldr1 max $ map length poss of
  1 -> concat poss
  _ -> possRed newPoss where
         (fxd',bad) = partition ((== 1) . length . fst) $ zip poss [0..]
         fxd = map (\(a,b) -> (head a,b)) fxd'
         redBad = map (\(a,b) -> (filter (not . (`elem` (map fst fxd))) a, b)) bad
         newPoss = map fst $ sortWith (snd) $ fxd' ++ redBad

depFields :: [String] -> [Int] -> Int
depFields fields tik = product $ map snd $ filter (("departure" `isPrefixOf`) . fst) $ zip fields tik

helper2 :: ([[(String,Int,Int,Int,Int)]], [Int]) -> Int
helper2 (poss, tik) = depFields fields tik where
  fields = map (\(str,_,_,_,_) -> str) $ possRed poss

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper2 . helper . inputP . grouper . lines   
    >> exitSuccess
--