module Day10Prob2 where

import System.Environment
import System.Exit
import System.IO
import Data.List

vectors = [(x,y) | x <- vals, y <- vals, x /= 0 || y /= 0] where
  vals = [-1,0,1]

access :: [String] -> (Int, Int) -> Char
access state (x,y) = state !! y !! x

tupComp :: (a,a) -> (a -> a -> Bool) -> (a, a) -> Bool
tupComp (x,y) comp (w,z) = (x `comp` w) && (y `comp` z)

checkDir :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [String] -> Bool
checkDir dims loc@(xLoc, yLoc) dir@(xDir, yDir) state = case (tupComp loc (>=) (0,0)) && (tupComp loc (<) dims) of
  False -> False
  True  -> continuation where
             newX   = xLoc + xDir
             newY   = yLoc + yDir
             newLoc = (newX, newY)
             newVal = case (tupComp newLoc (>=) (0,0)) && (tupComp newLoc (<) dims) of
                        False -> Nothing
                        True  -> Just $ (access state newLoc)
             continuation = case newVal of
                              Nothing  -> False
                              Just 'L' -> False
                              Just val -> val == '#' || (checkDir dims newLoc dir state)

checkAllD :: (Int, Int) -> (Int, Int) -> [String] -> Int
checkAllD dims loc state = length $ filter id checkLst where
  checkLst = map ((flip $ checkDir dims loc) state) vectors

updoot :: (Int, Int) -> [String] -> [String]
updoot dims@(_,y) state = map rowUpdoot [0..(y-1)] where
  rowUpdoot row = stateChange dims row state 

stateChange :: (Int, Int) -> Int -> [String] -> String
stateChange dims@(x,_) row state = map changeLoc [0..(x-1)]  where
  changeLoc xVal = changer dims (xVal, row) state

changer :: (Int, Int) -> (Int, Int) -> [String] -> Char
changer dims loc state = let val = access state loc in case val of
  '.' -> '.'
  'L' -> case checkAllD dims loc state == 0 of
           True -> '#'
           False -> 'L'
  '#' -> case checkAllD dims loc state < 5 of
           True -> '#'
           False -> 'L'

stateList :: [String] -> [[String]]
stateList state =  stateL where
  yDim   = length state
  xDim   = length $ head state
  dims   = (xDim, yDim)
  stateL = state : (map (updoot dims) stateL)

stationaryF :: [[String]] -> [String]
stationaryF lst = fst $ head $ dropWhile (\(a,b) -> a /= b) comprar where 
  comprar = zip lst $ drop 1 lst

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . length . filter (== '#') . concat . stationaryF . stateList . lines   
    >> exitSuccess
-- 