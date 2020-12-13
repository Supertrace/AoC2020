module Day13Prob2 where

import System.Environment
import System.Exit
import System.IO
import Data.List

splitter :: String -> [String]
-- splits the strings at commas
splitter []  = []
splitter str = a:(splitter rest) where
  (a,r) = break (== ',') str
  rest  = drop 1 r

inputP :: [String] -> [(Integer,Integer)]
-- parse the input into the useful bits
inputP [_,b] = busList where
  busList = map (\(a,b) -> (-a,read b)) $ filter ((/="x") . snd) $ zip [0..] $ splitter b

modInv :: Integer -> Integer -> Integer
-- inverts val mod base assuming that base is prime
modInv val base = (val^(base-2)) `mod` base

bM :: [(Integer,Integer)] -> Integer
-- number useful in solving the input system of modular equations
bM = product . map snd

n :: [(Integer,Integer)] -> [(Integer,Integer)]
-- list of numbers useful in solving the input system
n lst = map (\(a,b) -> let val = dM `div` b in (val, modInv val b)) lst where
  dM = bM lst

solver :: [(Integer, Integer)] -> (Integer,Integer)
-- solves the input system of modular equations
solver eqs = (sol, smallestSol) where
   sol = sum $ map (\(a,b) -> a * b) $ zip (map fst eqs) $ map (\(a,b) -> a*b) $ n eqs
   smallestSol = sol `mod` (bM eqs) 

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . solver . inputP . lines   
    >> exitSuccess
--