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

inserter :: Maybe Bool -> Maybe Bool
inserter val = case val of
  Just p -> Just $ not p
  _      -> Just True

updateBoard :: (Int, Int) -> Grid  -> Grid
updateBoard = M.alter inserter

helper :: [String] -> Int
helper input = result where
  locL   = map (pathEnd (0,0) . lineP) input
  afterG = foldr updateBoard M.empty locL
  result = length $ filter id $ M.elems afterG

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . lines   
    >> exitSuccess
--