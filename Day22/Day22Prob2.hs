module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import Data.Maybe
import qualified Data.HashSet as M

type Deck = [Int]

type Player = (Int, Deck)

type Plays = M.HashSet (Deck,Deck)

grouper :: [String] -> [[Int]]
grouper []  = []
grouper lns = (group) : (grouper rem) where 
                lsts  = break (== []) lns
                group = map read $ drop 1 $ fst lsts
                rem   = drop 1 $ snd lsts

addToDeck :: Deck -> Int -> Int -> Deck
addToDeck deck top bot = reverse $ bot:top:(reverse deck)

playRound :: (Player, Player, Plays) -> (Player, Player, Plays)
playRound ((p1,d1@(top1:r1)), (p2,d2@(top2:r2)), plays) = ((p1,newD1), (p2,newD2), newPlays) where
  newPlays = M.insert (d1, d2) plays
  (newD1,newD2) = case beats d1 d2 plays of
    True -> (addToDeck r1 top1 top2, r2)
    _    -> (r1, addToDeck r2 top2 top1)
  
beats :: Deck -> Deck -> Plays -> Bool
beats d1@(top1:r1) d2@(top2:r2) plays = case (d1,d2) `M.member` plays of
  True -> True
  _    -> case length r1 >= top1 && length r2 >= top2 of
            True -> p1Wins (1, (take top1 r1)) (2, (take top2 r2))
            _    -> top1 > top2

p1Wins :: Player -> Player -> Bool
p1Wins p1 p2 = p == 1 where
  (p,_) = winner p1 p2

winner :: Player -> Player -> Player
winner p1 p2 = head $ catMaybes $ map isDone $ iterate playRound (p1, p2, M.empty)

isDone :: (Player, Player, Plays) -> Maybe Player
isDone ((_,[]), p2, _)     = Just p2
isDone (p1, (_,[]), _)     = Just p1
isDone (p1@(_,d1), p2@(_,d2), plays) = case (d1,d2) `M.member` plays of
  True -> Just p1
  _    -> Nothing
  
scorer :: Player -> Int
scorer (_,d1) = score where
  score = sum $ map (\(a,b) -> a*b) $ zip [1..] $ reverse d1

helper :: [[Int]] -> Int
helper input = score where
  [p1,p2] = zip [1..] input
  score   = scorer $ winner p1 p2
  
main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . grouper . lines   
    >> exitSuccess
--