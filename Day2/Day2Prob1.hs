module Day2Prob1 where
-- Searches through file to find two numbers summing to 2020
-- and then mutliplies them together
-- usage: Day1Prob1 filename

import System.Environment
import System.Exit
import System.IO
import Data.List

header :: String -> (Int, Int)
header s = (read a, read bs) where 
  (a,b) = break (== '-') s
  bs    = drop 1 b

breakDown :: [[String]] -> [(Int,Int,Char,String)]
breakDown ([a,b,d]:rest) = ((int1, int2, head b, d) : breakDown rest) where (int1,int2) = header a
breakDown [] = []
breakDown _  = error "breakDown pattern matching failure"

checker :: (Int, Int, Char, String) -> Bool
checker (int1, int2, c, s) = let occur = length $ filter (== c) s in (occur >= int1) && (occur <= int2)
   

tally :: [(Int,Int,Char,String)] -> Int
tally = length . filter checker 

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . tally . breakDown . map words . lines >> exitSuccess
