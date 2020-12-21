module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import GHC.Exts
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

helper :: [[String]] -> String 
helper input   = str where
  pairingL     = map ingredientGrab input
  betterPair   = map (\(a,b) -> (a,allergenProcess b)) pairingL
  possAller    = foldr addFood M.empty betterPair
  allergenPosL = map (S.toList . snd) $ sortWith fst $ M.toList possAller
  (h:r)        = reverse $ possHep allergenPosL
  str          = concat $ reverse $ h : (map (reverse . (',':) . reverse) r)

possRed :: Eq a => [a] -> [[a]] -> Maybe [a]
possRed soFar []                   = Just soFar
possRed _     ([]:rest)            = Nothing
possRed soFar ((val:vals):rest) = case val `elem` soFar of
  True -> possRed soFar (vals:rest)
  _    -> case possRed (val:soFar) rest of
            Nothing -> possRed soFar (vals:rest)
            Just a  -> Just a
  
possHep :: Eq a => [[a]] -> [a] 
possHep poss = result where
  possLst = sortWith (length . fst) $ zip poss [0..]
  result  = case possRed [] (map fst possLst) of
              Just a  -> map snd $ sortWith (snd . fst) $ zip possLst $ reverse a
              Nothing -> error "possRed didn't work :("

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . map words . lines   
    >> exitSuccess
--