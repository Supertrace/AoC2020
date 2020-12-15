module Day15Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List
import qualified Data.HashMap.Strict as Map 

parse :: String -> [Int]
parse ""  = []
parse str = read a : (parse $ drop 1 b) where
  (a,b) = break (== ',') str

initialize :: [Int] -> (Map.HashMap Int Int, (Int, Int))
initialize lst = let newLst = zip lst [1..] in 
  (Map.fromList $ drop 1 $ reverse newLst, last newLst)

play :: Int -> Int -> Map.HashMap Int Int -> [Int]
play index prev mep = newVal : (((play $! (index + 1)) $! (newVal)) $! newMap) where
  newVal = case (Map.!?) mep prev of
             Just a -> index - a - 1
             _      -> 0
  newMap = Map.insert prev (index - 1) mep

startPlay :: (Map.HashMap Int Int, (Int, Int)) -> [Int]
startPlay (mep, (val, index)) = play (index + 1) val mep 

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . last . take 29999993 . startPlay . initialize . parse . head . lines   
    >> exitSuccess
-- 