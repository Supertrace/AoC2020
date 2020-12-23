module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.HashMap.Strict as M

type Circle = M.HashMap Int (Int,Int)

makeMove :: (Int,Circle) -> (Int,Circle)
makeMove (curr, circ) = (newLoc, resultC) where
  (prev,x) = circ M.! curr
  [a,b,c]  = cTake 3 x circ
  moveTo   = goal curr [a,b,c]
  newLoc   = last $ cTake 2 c circ
  nCirc    = M.insert newLoc (curr, last $ cTake 2 newLoc circ) $ M.insert curr (prev, newLoc) circ
  (pMoveTo,aftBl) = case nCirc M.!? moveTo of
                      Just a -> a
                      _      -> (moveTo-1,moveTo+1)
  aaftBl  = last $ cTake 2 aftBl nCirc
  resultC = M.insert aftBl (c, aaftBl) $ M.insert c (b,aftBl) $ M.insert a (moveTo,b) $ M.insert moveTo (pMoveTo, a) nCirc 

cTake :: Int -> Int -> Circle -> [Int]
cTake 0   _   _    = []
cTake num loc circ = loc : (cTake (num - 1) newLoc circ) where
  newLoc = case circ M.!? loc of
             Just (_, nxt) -> nxt
             _             -> loc + 1

goal :: Int -> [Int] -> Int
goal a lst = let newA = (a-1) `mod` 1000000 in case newA `elem` lst of
           True -> goal newA lst
           _    -> newA

mapConstruct :: Circle -> Maybe Int -> [Int] -> Circle
mapConstruct circ _    []         = circ       
mapConstruct circ prev (top:rest) = mapConstruct nCirc (Just top) rest where
  prevVal = case prev of
              Just a -> a
              _      -> 0
  nxt     = case rest of
              a:_ -> a
              _   -> 10
  nCirc   = M.insert top (prevVal, nxt) circ

mepUpdate :: M.HashMap Int v -> [(Int, v)] -> M.HashMap Int v
mepUpdate = foldr (\(k,v) circ -> M.insert k v circ)

helper :: [Int] -> (Integer, [Integer])
helper input  = (result, vals) where
  mep'   = mapConstruct M.empty Nothing input
  mep    = mepUpdate mep' [(0, (1000000-1, head input)), (1000000-1, (1000000-2,0)), (10, (last input, 11))]
  rMep   = snd $ head $ drop 10000000 $ iterate makeMove (head input, mep)
  vals   = map toInteger $ drop 1 $ cTake 3 1 rMep
  result = product vals

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . map (digitToInt) . head . lines   
    >> exitSuccess
--10000000