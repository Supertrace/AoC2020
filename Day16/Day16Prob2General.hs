module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import GHC.Exts

type Rule = (String,Int,Int,Int,Int)

grouper :: [String] -> [[String]]
grouper []  = []
grouper lns = (group) : (grouper rem) where 
                lsts  = break (== []) lns
                group = fst lsts
                rem   = drop 1 $ snd lsts

ruleParse :: String -> Rule
ruleParse str = (field, min1, max1, min2, max2) where
  (field , vals')   = break (== ':') str
  vals         = drop 2 vals'
  (rng1, rng2) = break (== ' ') vals
  realRng2 = drop 4 rng2
  ((min1, max1), (min2, max2)) = (minMax rng1, minMax realRng2)

minMax :: String -> (Int, Int)
minMax str = (read a, read b) where
  (a,b') = break (== '-') str
  b      = drop 1 b'

ticketParse :: String -> [Int]
ticketParse []  = []
ticketParse str = read a : ticketParse b where
  (a,b') = break (== ',') str
  b      = drop 1 b'

inputP :: [[String]] -> ([Rule], [Int], [[Int]])
inputP [rule, [_,tik], (_:tix)] = (map ruleParse rule, ticketParse tik, map ticketParse tix)

ruleC :: (String,Int,Int,Int,Int) -> Int -> Bool
ruleC (_,min1,max1,min2,max2) val = check val min1 max1 || check val min2 max2 where 
  check val smol big = val >= smol && val <= big

isValid :: [Rule] -> Int -> Bool
isValid rules val = any (flip ruleC val) rules

checker :: [Rule] -> [[Int]] -> [[Int]]
checker rules vals = filter (all (isValid rules)) vals

fieldF :: [[Rule]] -> [[Int]] -> [[Rule]]
fieldF possFields []         = possFields
fieldF possFields (tik:rest) = fieldF newPos rest where
  zpped = zip possFields tik
  newPos = map (fieldWinnow) zpped
  fieldWinnow :: ([Rule], Int) -> [Rule]
  fieldWinnow ([rule], _)  = [rule]
  fieldWinnow (rules, val) = filter (flip ruleC val) rules

helper :: ([Rule], [Int], [[Int]]) -> ([[Rule]], [Int])
helper (rules, tik, vals@(val:_)) = (fieldF startPoss validVals, tik) where
  startPoss = map snd $ zip val $ iterate id rules
  validVals = checker rules vals

possRed :: [Rule] -> [[Rule]] -> Maybe [Rule]
-- depth first search of picking one Rule from each list so that all rules picked
-- are different and each list has one rule picked from it
possRed soFar []                   = Just soFar
possRed _     ([]:rest)            = Nothing
possRed soFar ((rule:oRules):rest) = case rule `elem` soFar of
  True -> possRed soFar (oRules:rest)
  _    -> case possRed (rule:soFar) rest of
            Nothing -> possRed soFar (oRules:rest)
            Just a  -> Just a
  
possHep :: [[Rule]] -> [Rule] 
possHep poss = result where
  possLst = sortWith (length . fst) $ zip poss [0..]
  result  = case possRed [] (map fst possLst) of
              Just a  -> map snd $ sortWith (snd . fst) $ zip possLst $ reverse a
              Nothing -> error "possRed didn't work :("

depFields :: [String] -> [Int] -> Int
depFields fields tik = product $ map snd $ filter (("departure" `isPrefixOf`) . fst) $ zip fields tik

helper2 :: ([[Rule]], [Int]) -> Int
helper2 (poss, tik) = depFields fields tik where
  fields = map (\(str,_,_,_,_) -> str) $ possHep poss

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper2 . helper . inputP . grouper . lines   
    >> exitSuccess
--