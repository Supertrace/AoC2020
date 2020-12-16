module Main where

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

ruleParse :: String -> (Int, Int, Int, Int)
ruleParse str = (min1, max1, min2, max2) where
  (_, vals')   = break (== ':') str
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

inputP :: [[String]] -> ([(Int,Int,Int,Int)], [[Int]])
inputP [rule, _, (_:tix)] = (map ruleParse rule, map ticketParse tix)

ruleC :: (Int,Int,Int,Int) -> Int -> Bool
ruleC (min1,max1,min2,max2) val = (val >= min1 && val <= max1) || (val >= min2 && val <= max2)

isValid :: [(Int,Int,Int,Int)] -> Int -> Bool
isValid rules val = any (flip ruleC val) rules

checker :: [(Int,Int,Int,Int)] -> [[Int]] -> [Int]
checker rules vals = filter (not . isValid rules) vals' where
  vals' = concat vals
  
main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . sum . uncurry checker . inputP . grouper . lines   
    >> exitSuccess
--