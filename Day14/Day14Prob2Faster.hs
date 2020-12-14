module Day14Prob2F where

import System.Environment
import System.Exit
import System.IO
import Data.List

grouper :: [String] -> [[String]]
grouper []  = []
grouper (mask:lns) = (mask:group) : (grouper rem) where
                lsts  = break ("mask =" `isPrefixOf`) lns
                group = fst lsts
                rem   = snd lsts

maskParse :: String -> String
maskParse = drop 2 . snd . break (== '=')

memParse :: String -> (Integer, Integer)
memParse memStat = (read memNum, val) where
  start = drop 1 $ dropWhile (/= '[') memStat
  (memNum, end) = break (== ']') start
  val = read $ dropWhile (not . (`elem` ['0'..'9'])) end

grpParse :: [String] -> (String, [(Integer, Integer)])
grpParse (mask:memStuff) = (reverse $ maskParse mask, map memParse memStuff)

toBin :: Integer -> String
toBin 0   = ""
toBin num = charM : toBin d where
  (d, m) = num `divMod` 2
  charM  = case m of
    0 -> '0'
    1 -> '1'

comparer :: (Char, Char) -> Char
comparer (a,b) = case a of
  '0' -> b
  '1' -> '1'
  _   -> 'x'

possGen :: Char -> String
possGen a = case a of
  'x' -> "01"
  _   -> [a]

possList :: [String] -> [String]
possList [str] = [[a] | a <- str]
possList (poss:rest) = concat [map f (possList rest) | f <- map (\a -> (a :)) poss]

masker :: String -> Integer -> String
masker mask val = newVals where
  exVal   = (toBin val) ++ (iterate id '0')
  newVals = map comparer $ zip mask $ exVal

matches :: String -> String -> Bool
matches s1 s2 = all charMatch $ zip s1 s2 where
  charMatch :: (Char, Char) -> Bool
  charMatch (c1,c2) = case c1 == c2 of
    True -> True
    _    -> (c1 == 'x') || (c2 == 'x')

prevAdder :: [String] -> (String, [String], Integer) -> Integer
prevAdder []             (pat, lst, val)   = val *  (toInteger $ length lst)
prevAdder _              (_, [], _)        = 0
prevAdder (toMatch:rest) a@(pat, lst, val) = case pat `matches` toMatch of
  True  -> prevAdder rest (pat, newL, val) where
             newL = filter (not . (`matches` toMatch)) lst
  _     -> prevAdder rest a

prevL :: [String] -> Integer -> [(String, Integer)] -> ([String], Integer)
prevL strs num []                = (strs, num)
prevL strs num ((pat, val):coms) = prevL newPrev (num + outcome) coms where
  outcome = prevAdder strs (pat, possList $ map possGen pat, val)
  newPrev = case outcome of
              0 -> strs
              _ -> pat : strs 

memList :: [String] -> Integer -> [(String, [(Integer,Integer)])] -> Integer
memList _    total []                  = total
memList prev total ((mask, mems):rest) = memList newPrev newTot rest where
  newMems           = reverse mems
  newVals           = map (\(a,b) -> (masker mask a, b)) newMems
  (newPrev, newTot) = prevL prev total newVals

  
main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . memList [] 0 . reverse . map grpParse . grouper . lines
    >> exitSuccess
-- 