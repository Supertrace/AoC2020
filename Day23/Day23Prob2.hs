module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.HashMap.Strict as M

type Circle = M.HashMap Int Int

makeMove :: (Int,Int,Circle) -> (Int,Int,Circle)
makeMove (prev, curr, circ) = (curr, newLoc, resultC) where
  x = case circ M.!? curr of
        Just a  -> a
        Nothing -> curr + 1
  [a,b,c] = cTake 3 x circ
  moveTo  = goal curr [a,b,c]
  newLoc  = last $ cTake 2 c circ
  aftBl   = case circ M.!? moveTo of
              Just a -> a
              _      -> moveTo+1
  resultC = M.insert c aftBl $ M.insert moveTo a $ M.insert curr newLoc circ 

cTake :: Int -> Int -> Circle -> [Int]
cTake 0   _   _    = []
cTake num loc circ = loc : (cTake (num - 1) newLoc circ) where
  newLoc = case circ M.!? loc of
             Just nxt -> nxt
             _        -> loc + 1

goal :: Int -> [Int] -> Int
goal a lst = let newA = (a-1) `mod` 1000000 in case newA `elem` lst of
           True -> goal newA lst
           _    -> newA

mapConstruct :: Circle -> [Int] -> Circle
mapConstruct circ []         = circ       
mapConstruct circ (top:rest) = mapConstruct nCirc rest where
  nxt     = case rest of
              a:_ -> a
              _   -> 10
  nCirc   = M.insert top nxt circ

mepUpdate :: M.HashMap Int v -> [(Int, v)] -> M.HashMap Int v
mepUpdate = foldr (\(k,v) circ -> M.insert k v circ)

helper :: [Int] -> (Integer, [Integer])
helper input  = (result, vals) where
  mep'   = mapConstruct M.empty input
  mep    = mepUpdate mep' [(0, head input), (1000000-1, 0)]
  rMep   = (\(_,_,c) -> c) $ head $ drop 10000000 $ iterate makeMove (0, head input, mep)
  vals   = map toInteger $ drop 1 $ cTake 3 1 rMep
  result = product vals

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . map (digitToInt) . head . lines   
    >> exitSuccess
--10000000