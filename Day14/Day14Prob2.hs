module Day14Prob2 where

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

deBin :: String -> Integer
deBin str = sum $ map value zipped where
  zipped = zip str [0..]
  value (a,b) = (read [a] :: Integer) * (2^b)

comparer :: (Char, Char) -> String
comparer (a,b) = case a of
  '0' -> [b]
  '1' -> "1"
  _   -> "01"

possList :: [String] -> [String]
possList [str] = [[a] | a <- str]
possList (poss:rest) = concat [map f (possList rest) | f <- map (\a -> (a :)) poss]  

masker :: String -> Integer -> [Integer]
masker mask val = newVals where
  exVal   = (toBin val) ++ (iterate id '0')
  poss    = map comparer $ zip mask $ exVal
  newVals = map deBin $ possList poss

prevAdder :: [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)]
prevAdder _    []                 = []
prevAdder prev (val@(reg,_):rest) = case reg `elem` prev of
  True  -> prevAdder prev rest
  False -> val : prevAdder (reg:prev) rest 

memList :: [(Integer,Integer)] -> [(String, [(Integer,Integer)])] -> [(Integer, Integer)]
memList prev []                  = prev
memList prev ((mask, mems):rest) = memList (prev ++ toAdd) rest where
  newMems = reverse mems
  newVals = concat $ map (\(a,b) -> [(newA, b) | newA <- masker mask a]) newMems
  savedI  = map fst prev
  toAdd   = prevAdder savedI newVals

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . sum . map snd . memList [] . reverse . map grpParse . grouper . lines
    >> exitSuccess
--