module Day12Prob2 where

import System.Environment
import System.Exit
import System.IO
import Data.List

changeState :: (Int, Int, Int, Int) -> String -> (Int, Int, Int, Int)
changeState (wayX, wayY, xLoc, yLoc) (com:v) = let val = read v in case com of
  'N' -> (wayX, wayY + val, xLoc, yLoc)
  'S' -> (wayX, wayY - val, xLoc, yLoc)
  'E' -> (wayX  + val, wayY, xLoc, yLoc)
  'W' -> (wayX - val, wayY, xLoc, yLoc)
  'L' -> let (newX,newY) = newDir (wayX,wayY) val in (newX, newY, xLoc, yLoc)
  'R' -> let (newX,newY) = newDir (wayX,wayY) (-val) in (newX, newY, xLoc, yLoc)
  'F' -> (wayX, wayY, xLoc + (wayX * val), yLoc + (wayY * val))

dirList = zip ['e', 'n', 'w', 's'] [0..]

newDir :: (Int, Int) -> Int -> (Int, Int)
newDir (x,y) val = newLoc where
  shifts = val `div` 90
  newLoc = case shifts `mod` 4 of
    0 -> (x, y) 
    1 -> (-y,x)
    2 -> (-x, -y)
    3 -> (y, -x)

endState :: [String] -> (Int, Int ,Int, Int)
endState = foldl changeState (10,1,0,0) 

distanceF :: [String] -> Int
distanceF lst = (abs x) + (abs y) where
  (_, _, x, y) = endState lst

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . distanceF . lines   
    >> exitSuccess
--