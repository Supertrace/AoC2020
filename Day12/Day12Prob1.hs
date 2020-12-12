module Day12Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

changeState :: (Char, Int, Int) -> String -> (Char, Int, Int)
changeState (dir, xLoc, yLoc) (com:v) = let val = read v in case com of
  'N' -> (dir, xLoc, yLoc + val)
  'S' -> (dir, xLoc, yLoc - val)
  'E' -> (dir, xLoc + val, yLoc)
  'W' -> (dir, xLoc - val, yLoc)
  'L' -> (newDir dir val, xLoc, yLoc)
  'R' -> (newDir dir (-val), xLoc, yLoc)
  'F' -> (dir, xLoc + (xChange dir val), yLoc + (yChange dir val))

dirList = zip ['e', 'n', 'w', 's'] [0..]

newDir :: Char -> Int -> Char
newDir dir val = fst $ dirList !! newNum where
  shifts = val `div` 90
  dirNum = snd $ head $ dropWhile ((/= dir) . fst) dirList
  newNum = (dirNum + shifts) `mod` 4

xChange :: Char -> Int -> Int
xChange dir val = case dir of
  'e' -> val
  'n' -> 0
  'w' -> -val
  's' -> 0

yChange :: Char -> Int -> Int
yChange dir val = case dir of
  'e' -> 0
  'n' -> val
  'w' -> 0
  's' -> -val

endState :: [String] -> (Char,Int, Int)
endState = foldl changeState ('e',0,0) 

--stateLister :: (Char, Int, Int) -> [String] -> [(Char, Int, Int)]


distanceF :: [String] -> Int
distanceF lst = (abs x) + (abs y) where 
  (_, x, y) = endState lst

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . distanceF . lines   
    >> exitSuccess
--