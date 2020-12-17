module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import qualified Data.HashMap.Strict as M

type Board = M.HashMap (Int,Int,Int) Char

initialize :: [String] -> (Int, Int, Board)
initialize rows = (x, y, mapConst rows' M.empty) where
  rows' = flip zip [0..] $ map (zip [0..]) rows
  y     = snd $ last rows'
  x     = fst $ last $ fst $ head rows'

mapConst :: [([(Int,Char)], Int)] -> Board -> Board
mapConst []              mep = mep
mapConst ((row, y):rest) mep = mapConst rest (M.union mep newMep) where
  rowList = map (\(x,c) -> ((x,y,0), c)) row
  newMep  = M.fromList rowList

surrounding :: (Int, Int, Int) -> Board -> String
surrounding loc@(x,y,z) board = unMaybe $ map (`M.lookup` board) otherLocs where
  otherLocs' = [(x',y',z') | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1]]
  otherLocs  = filter (/= loc) otherLocs'
  
unMaybe :: [Maybe a] -> [a]
unMaybe []         = []
unMaybe (val:rest) = case val of
  Just a -> a : unMaybe rest
  _      -> unMaybe rest 

update :: Board -> (Int, Int, Int) -> ((Int,Int,Int), Char)
update board loc = let environ = surrounding loc board in case loc `M.lookup` board of
  Just '#' -> case (length $ filter (== '#') environ) `elem` [2,3] of
                True -> (loc, '#')
                _    -> (loc, '.')
  _        -> case (length $ filter (== '#') environ) == 3 of
                True -> (loc, '#')
                _    -> (loc, '.')

oneStep :: (Int, Int, Int, Board) -> (Int, Int, Int, Board)
oneStep (x, y, steps, board) = (x, y, steps + 1, newBoard) where
  lowerB   = -steps
  range    = [(x,y,z) | x <- [lowerB..(x + steps)], y <- [lowerB..(y + steps)], z <- [lowerB..(-lowerB)]]
  newBoard =  M.fromList $ map (update board) range

helper :: [String] -> [(Int, Int, Int, Board)]
helper input = iterate oneStep (x,y,1, board) where
  (x,y, board) = initialize input

counter :: (Int, Int, Int, Board) -> Int
counter (x,y,steps, board) = length $ filter (== '#') vals where
  lowerB   = -steps
  range    = [(x,y,z) | x <- [lowerB..(x + steps)], y <- [lowerB..(y + steps)], z <- [lowerB..(-lowerB)]]
  vals     = unMaybe $ map (`M.lookup` board) range

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . counter . last . take 7 . helper . lines   
    >> exitSuccess
--