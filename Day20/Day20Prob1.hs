module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

type Tile = (Int, M.HashMap (Int, Int) Char)

grouper :: [String] -> [[String]]
grouper []  = []
grouper lns = (group) : (grouper rem) where 
                lsts  = break (== []) lns
                group = fst lsts
                rem   = drop 1 $ snd lsts

tileGen :: [String] -> M.HashMap (Int, Int) Char
tileGen strs = M.fromList $ map (\(c,x,y) -> ((x,y),c)) $ concat $ map yCoord coordList where
  coordList = flip zip [0..] $ map (flip zip [0..]) strs
  yCoord :: ([(Char,Int)], Int) -> [(Char,Int,Int)]
  yCoord ([],_) = []
  yCoord ((char, x):rest, y) = (char, x, y) : yCoord (rest, y)

tileParse :: [String] -> Tile
tileParse (title:grid) = (num, tileGen grid) where
  num = read $ reverse $ drop 1 $ reverse $ drop 1 $ dropWhile (/= ' ') title

--rotate :: Tile -> Tile
--rotate tile@(title,mep) = (title, M.mapWithKey (\((x,y), _) -> mep M.! (y, 9 - x)))

--flip :: Tile -> Tile
--flip tile@(title,mep) = (title, M.mapWithKey (\((x,y), _) -> mep M.! (9 - x, y)))

--d8 = [[id], [flip], [rotate], [rotate, flip], [rotate, rotate], [rotate,rotate,flip], [rotate,rotate,rotate], [rotate,rotate,rotate,flip]]

--match :: Tile -> Tile -> Bool
--match tile1 tile2 = any (exactMatch tile1) $ map (foldr ($) tile2) d8

match :: Tile -> Tile -> Bool
match tile1 tile2 = any (`elem` ls2) ls1' where
  ls1  = getTileLines tile1
  ls1' = ls1 ++ (map reverse ls1)
  ls2  = getTileLines tile2

sMatch :: String -> Tile -> Bool
sMatch side tile = side `elem` ls where
  ls' = getTileLines tile
  ls  = ls' ++ (map reverse ls')

sideMatches :: [Tile] -> Tile -> Int
sideMatches tiles tile = result where
  sides = getTileLines tile
  result = length $ filter ((> 1) . length) $ map (\side -> filter (sMatch side) tiles) sides

getTileLines :: Tile -> [String]
getTileLines (_, mep) = [top, lside, rside, bottom] where
  top = map (mep M.!) [(x,0) | x <- [0..9]]
  lside = map (mep M.!) [(0,y) | y <- [0..9]]
  rside = map (mep M.!) [(9,y) | y <- [0..9]]
  bottom = map (mep M.!) [(x,9) | x <- [0..9]]

isCornerTile :: [Tile] -> Tile -> Bool
isCornerTile tiles tile = sideMatches tiles tile == 2

helper :: [[String]] -> Int
helper inputs = val where
  tiles = map tileParse inputs
  cornerTiles = filter (isCornerTile tiles) tiles
  val = product $ map (\(num, _) -> num) cornerTiles

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . grouper . lines   
    >> exitSuccess
--