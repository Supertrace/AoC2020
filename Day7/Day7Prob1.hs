module Day7Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List
import qualified Data.Set.Strict as S
import qualified Data.HashMap.Strict as M

type BagRule = (String, [(Int,String)])
type UpBags = M.HashMap String (S.Set String)

bag :: [String] -> [(Int,String)]
bag [] = []
bag list = (num, rest) : (bag newList) where
  (c,d)   = span (\a -> (take 3 a) /= "bag") list
  newList = drop 1 d
  num     = let newC = head c in case newC of
    "no" -> 0
    _    -> (read $ head c) :: Int
  rest = concat $ drop 1 c

separate :: String -> BagRule
separate str = let (a,b) = span (/= "bags") (words str) in (concat a, bag $ drop 2 b)

oneLevelUp :: String -> [(String, [(Int, String)])] -> [(String, [(Int, String)])]
oneLevelUp bagC bagList  = filter (\(a,b) -> bagC `elem` (map snd b)) bagList

bagCounter :: [String] -> [String] -> [(String, [(Int, String)])] -> [String]
bagCounter _ [] _ = []
bagCounter soFar stillCheck@(s:rest) bagL = (s : bagCounter (s : soFar) newCheck bagL) where
  upLst    = map fst $ oneLevelUp s bagL
  newLst   = [s | s <- upLst, not (s `elem` (soFar ++ stillCheck))]
  newCheck = newLst ++ rest

update :: BagRule -> UpBags -> UpBags
update (name, rlList) bags = foldr (flip M.insertWith name) bags
  innerBags = map snd rlList

upBagger :: [BagRule] -> UpBags
upBagger = foldr update M.empty

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . flip (-) 1 . length . bagCounter [] ["shinygold"] . map separate . lines   
    >> exitSuccess
--