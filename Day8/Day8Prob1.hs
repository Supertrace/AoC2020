module Day8Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List
import qualified Data.HashMap.Lazy as M

type Prog = M.HashMap Int Int

exec :: (Int, Int) -> (String, Int) -> (Int,Int)
exec (acum, loc) (s, val) = case s of
  "nop" -> (acum, loc + 1)
  "acc" -> (acum + val, loc + 1)
  "jmp" -> (acum, loc + val)

parse :: String -> (String, Int)
parse s = let (a,b) = break (== ' ') s in (a, numEval $ drop 1 b)

numEval :: String -> Int
numEval str@(s:t) = case s of
  '-' -> read str
  _   -> read t

run :: (Int, Int) -> [(String,Int)] -> [(Int, Int)]
run state@(_, loc) prog = let newState = exec state $ prog !! loc in newState : (run newState prog)

loopF :: [Int] -> [(Int,Int)] -> Int
loopF tried ((a,b):s) = case b `elem` tried of
  True  -> a
  False -> loopF (b:tried) s

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . loopF [] . run (0,0) . map parse . lines   
    >> exitSuccess
--