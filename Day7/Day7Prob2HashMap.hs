module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import qualified Data.HashMap.Strict as M

type BagL = M.HashMap String [(Int, String)]

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

newBagCounter :: Int -> String -> BagL -> Int
newBagCounter _ "other" _   = 0 
newBagCounter num bagC bagL = (+) num $ (*) num $ (sum outcomes) where
  outcomes = map ((flip $ uncurry newBagCounter) bagL) $ bagL M.! bagC

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . flip (-) 1 . newBagCounter 1 "shinygold" . M.fromList . map separate . lines   
    >> exitSuccess
--