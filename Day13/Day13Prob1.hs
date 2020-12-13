module Day13Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

splitter :: String -> [String]
splitter []  = []
splitter str = a:(splitter rest) where
  (a,r) = break (== ',') str
  rest  = drop 1 r

inputP :: [String] -> (Int, [Int])
inputP [a,b] = (read a, busList) where
  busList = map read $ filter (/="x") $ splitter b

comparer :: (Int, Int) -> (Int, Int) -> (Int, Int)
comparer b1@(_,b) b2@(_,d) = case b <= d of
  True  -> b1
  False -> b2

differencer :: (Int, [Int]) -> Int
differencer (time, busIDs) = bID * arrivT where
  times         = zip busIDs $ map ((-time) `mod`) busIDs
  (bID, arrivT) = foldr1 comparer times

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . differencer . inputP . lines   
    >> exitSuccess
--