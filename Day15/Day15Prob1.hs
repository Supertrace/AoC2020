module Day15Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

parse :: String -> [Int]
parse ""  = []
parse str = read a : (parse $ drop 1 b) where
  (a,b) = break (== ',') str

nextTerm :: [(Int, Int)] -> Int
nextTerm (a:b:rest) = snd a - snd b
nextTerm _          = 0

play :: [Int] -> [Int]
play lst = next : play newList where
  term = last lst
  pos = reverse $ filter ((== term) . fst) $ zip lst [0..]
  next = nextTerm pos
  newList = lst ++ [next]

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . last . take 2013 . play . parse . head . lines   
    >> exitSuccess
--