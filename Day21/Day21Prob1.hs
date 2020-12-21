module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

type IngredPoss = M.HashMap String (S.Set String)

ingredientGrab :: [String] -> ([String],[String])
ingredientGrab = break ((== '(') . head)

allergenProcess :: [String] -> [String]
allergenProcess str = allergens where
  allergens = map (reverse . drop 1 . reverse) $ drop 1 str

addFood :: ([String], [String]) -> IngredPoss -> IngredPoss
addFood (ingreds, allergens) poss = newPoss where
  ingredS            = S.fromList ingreds
  combiner aller set = M.insertWith S.intersection aller ingredS set
  newPoss            = foldr combiner poss allergens

helper :: [[String]] -> Int
helper input = result where
  pairingL   = map ingredientGrab input
  betterPair = map (\(a,b) -> (a,allergenProcess b)) pairingL
  possAller  = foldr addFood M.empty betterPair
  allPossAller = S.unions $ M.elems possAller
  foodList = map fst betterPair
  result = length $ concat $ map (filter (not .(`S.member` allPossAller))) foodList
  = M. 

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . map words . lines   
    >> exitSuccess
--