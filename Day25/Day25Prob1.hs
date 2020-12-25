module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List

oneStep :: Integer -> Integer -> Integer
oneStep val sub = (flip mod 20201227) $! (val * sub)

helper :: [String] -> Integer
helper input = result where
  [card,door] = map read input
  vals = iterate (flip oneStep 7) 1
  doorLoop = snd $ head $ dropWhile ((/= door) . fst) $ zip vals [0..]
  result = head $ drop doorLoop $ iterate (flip oneStep card) 1

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . lines   
    >> exitSuccess
--