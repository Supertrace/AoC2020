module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List

type Deck = [Int]

type Player = (Int, Deck)

grouper :: [String] -> [[Int]]
grouper []  = []
grouper lns = (group) : (grouper rem) where 
                lsts  = break (== []) lns
                group = map read $ drop 1 $ fst lsts
                rem   = drop 1 $ snd lsts

addToDeck :: Deck -> Int -> Int -> Deck
addToDeck deck top bot = reverse $ bot:top:(reverse deck)

playRound :: (Player, Player) -> (Player, Player)
playRound ((p1,(top1:d1)), (p2,(top2:d2))) = ((p1,newD1), (p2,newD2)) where
  (newD1,newD2) = case top1 < top2 of
    True -> (d1, addToDeck d2 top2 top1)
    _    -> (addToDeck d1 top1 top2, d2)

scorer :: (Player, Player) -> Int
scorer ((_,d1),(_,d2)) = score where
  winDeck = case d1 of
              [] -> d2
              _  -> d1
  score = sum $ map (\(a,b) -> a*b) $ zip [1..] $ reverse winDeck

helper :: [[Int]] -> Int
helper input = score where
  [p1,p2] = zip [1..] input
  score   = scorer $ head $ dropWhile (\((_,a),(_,b)) -> a /= [] && b /= []) $ iterate playRound (p1,p2)
  
main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . grouper . lines   
    >> exitSuccess
--