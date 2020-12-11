module Day10Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

chunker :: String -> [String]
chunker (a:b:c:str) = [a,b,c] : chunker (b:c:str)
chunker _           = []  

updoot :: String -> [String] -> [String]
updoot prevR (row:next:rest) = (stateChange prevR row next) : updoot row (next:rest)
updoot prevR (row:[])        = stateChange prevR row ['.' | x <- row] : []
updoot _     []              = error "oops"

stateChange :: String -> String -> String -> String
stateChange prev row next = map changer chunked  where
  chunkier = chunker . ('.' :) . (++ ['.'])
  (cP,cR,cN) = (chunkier prev, chunkier row, chunkier next)
  chunked    = zip3 cP cR cN

changer :: (String, String, String) -> Char
changer ((a:b:[c]), (d:e:[f]), (g:h:[i])) = let adj = [a,b,c,d,f,g,h,i] in case e of
  '.' -> '.'
  'L' -> case filter (== '#') adj == [] of
           True -> '#'
           False -> 'L'
  '#' -> case (< 4) $ length $ filter (== '#') adj of
           True -> '#'
           False -> 'L'

stateList :: [String] -> [[String]]
stateList lst@(a:_) =  stateL where
  top = ['.' | x <- a]
  stateL = lst : (map (updoot top) stateL)

stationaryF :: [[String]] -> [String]
stationaryF lst = fst $ head $ dropWhile (\(a,b) -> a /= b) comprar where 
  comprar = zip lst $ drop 1 lst

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . length . filter (== '#') . concat . stationaryF . stateList . lines   
    >> exitSuccess
-- 