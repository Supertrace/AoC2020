module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import qualified Data.HashMap.Strict as M

type Grid = M.HashMap (Int,Int) Bool

lineP :: String -> [String]
lineP []       = []
lineP (a:rest) = instructions : lineP remain where
  (instructions,remain) = case a `elem` ['e','w'] of
    True -> ([a], rest)
    _    -> (a : take 1 rest, drop 1 rest)

pathEnd :: (Int,Int) -> [String] -> (Int, Int)
pathEnd loc   []          = loc
pathEnd (x,y) (inst:rest) = pathEnd result rest where
  result = case inst of
             "e"  -> (x+2, y)
             "w"  -> (x-2, y)
             "ne" -> (x+1, y+1)
             "nw" -> (x-1, y+1)
             "sw" -> (x-1, y-1)
             "se" -> (x+1, y-1)

neighbors :: (Int, Int) -> [(Int,Int)]
neighbors loc = map (pathEnd loc) [["e"],["w"],["ne"],["nw"],["se"],["sw"]]

inserter :: Maybe Bool -> Maybe Bool
inserter val = case val of
  Just p -> Just $ not p
  _      -> Just True

updateBoard :: (Int, Int) -> Grid  -> Grid
updateBoard = M.alter inserter

createBoard :: [String] -> (Grid,Int,Int)
createBoard input = (afterG,maxX,maxY) where
  locL   = map (pathEnd (0,0) . lineP) input
  afterG = foldr updateBoard M.empty locL
  locs   = M.keys afterG
  maxX   = foldr1 max $ map (abs.fst) locs
  maxY   = foldr1 max $ map (abs.snd) locs 

update :: Grid -> (Int, Int) -> ((Int,Int), Bool)
update board loc = (loc, val) where
  environ = (map (board M.!?) $ neighbors loc)
  len = length $ filter (== Just True) environ
  val = case loc `M.lookup` board of
    Just True -> case len == 0 || len > 2 of
                   True -> False
                   _    -> True
    _         -> case len == 2 of
                   True -> True
                   _    -> False

oneStep :: (Int, Int, Grid) -> (Int, Int, Grid)
oneStep (maxX, maxY, board) = (newmx, newmy, newBoard) where
  (newmx,newmy) = (maxX +1, maxY + 1)
  range    = [(x,y) | x <- [-newmx..newmx], y <- [-newmy..newmy], (x `mod` 2) == (y `mod` 2)]
  newBoard =  M.fromList $ map (update board) range

helper :: [String] -> Int
helper input = numNGrid where
  (grid,maxX,maxY) = createBoard input
  nGrid = head $ drop 100 $ iterate oneStep (maxX,maxY, grid)
  numNGrid = length $ filter id $ M.elems $ (\(_,_,c) -> c) nGrid

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . lines   
    >> exitSuccess
--