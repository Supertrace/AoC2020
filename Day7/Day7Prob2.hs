module Day7Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List

bag :: [String] -> [(Int,String)]
bag [] = []
bag list = (num, rest) : (bag newList) where
  (c,d) = span (\a -> (take 3 a) /= "bag") list
  newList = drop 1 d
  num =  let newC = head c in case newC of
    "no" -> 0
    _    -> (read $ head c) :: Int
  rest = concat $ drop 1 c

separate :: String -> (String, [(Int, String)])
separate str = let (a,b) = span (/= "bags") (words str) in (concat a, bag $ drop 2 b)

oneLevelUp :: String -> [(String, [(Int, String)])] -> [(String, [(Int, String)])]
oneLevelUp bagC bagList  = filter (\(a,b) -> bagC `elem` (map snd b)) bagList

oneLevelDown :: String -> [(String, [(Int, String)])] -> [(Int, String)]
oneLevelDown bagC bagList  = case find ((== bagC) . fst) bagList of
  Just (a,b) -> b
  Nothing -> error "oneLevelDown went oops"

newBagCounter :: Int -> String -> [(String, [(Int, String)])] -> Int
newBagCounter _ "other" _   = 0 
newBagCounter num bagC bagL = (+) num $ (*) num $ (sum outcomes) where
  outcomes = map ((flip $ uncurry newBagCounter) bagL) $ oneLevelDown bagC bagL

bagCounter :: [String] -> [String] -> [(String, [(Int, String)])] -> [String]
bagCounter _ [] _ = []
bagCounter soFar stillCheck@(s:rest) bagL = (s : bagCounter (s : soFar) newCheck bagL) where
  upLst = map fst $ oneLevelUp s bagL
  newLst = [s | s <- upLst, not (s `elem` (soFar ++ stillCheck))]
  newCheck = newLst ++ rest

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . flip (-) 1 . newBagCounter 1 "shinygold" . map separate . lines   
    >> exitSuccess
--