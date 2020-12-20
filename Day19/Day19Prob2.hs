module Day6Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List
import qualified Data.HashMap.Strict as M

type RuleSet = M.HashMap Int Rule
data Rule = Or [Int] (Maybe [Int]) | Val Char
  deriving (Show)

grouper :: [String] -> [[String]]
grouper []  = []
grouper lns = (group) : (grouper rem) where 
                lsts  = break (== []) lns
                group = fst lsts
                rem   = drop 1 $ snd lsts

ruleParse :: String -> (Int, Rule)
ruleParse str = (read rulNum, rule) where 
  (rulNum, rules') = break (== ':') str
  rules = drop 2 rules'
  rule = case head rules of
            '"' -> Val $ head $ drop 1 rules
            _   -> Or (ruleConv rule1) rule2 where 
                     (rule1, rule2') = break (== '|') rules
                     ruleConv        = map read . words
                     rule2           = case drop 2 rule2' of
                                         [] -> Nothing
                                         a  -> Just $ ruleConv a

ruleInit :: [String] -> RuleSet
ruleInit = M.fromList . map ruleParse

match :: RuleSet -> [Int] -> String -> Bool
match _     []               ""  = True
match _     []               str = False
match rules (ruleNum:oRules) str = result where
  rule = case M.lookup ruleNum rules of
           Just val -> val
           Nothing  -> error "Rule not in RuleSet"
  result = case rule of
    Val a            -> case str of
                          (b:rest) | a == b -> match rules oRules rest
                          _                 -> False
    Or rules1 rules2 -> case match rules (rules1 ++ oRules) str of
                          False -> case rules2 of
                                     Just r -> match rules (r ++ oRules) str
                                     _      -> False
                          True  -> True

helper :: [[String]] -> Int
helper [rules, patts] = length $ filter id tryMatch where
  ruleSet' = ruleInit rules
  ruleSet  = M.insert 8 (Or [42] $ Just [42,8]) $ M.insert 11 (Or [42,31] $ Just [42,11,31]) ruleSet'
  tryMatch = map (match ruleSet [0]) patts

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . grouper . lines   
    >> exitSuccess
--