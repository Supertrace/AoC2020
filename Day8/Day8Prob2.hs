module Day8Prob2 where

import System.Environment
import System.Exit
import System.IO
import Data.List

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

run :: Int -> (Int, Int) -> [(String,Int)] -> [(Int, Int)]
run len state@(_, loc) prog = case loc < len of
  True  -> let newState = exec state $ prog !! loc in newState : (run len newState prog)
  False -> []

loopF :: [Int] -> [(Int,Int)] -> Maybe Int
loopF tried []        = Nothing
loopF tried (state:s) = let (a,b) = state in case b `elem` tried of
  True  -> Just a
  False -> loopF (b:tried) s

possGen :: [(String,Int)] -> [(String, Int)] -> [[(String, Int)]]
possGen prev ((inst, val) : rest) = case inst of
  "nop" -> (prev ++ (("jmp", val) : rest)) : (possGen (prev++[(inst,val)]) rest)
  "jmp" -> (prev ++ (("nop", val) : rest)) : (possGen (prev++[(inst,val)]) rest)
  _     -> possGen (prev++[(inst,val)]) rest

evalPoss :: [(String, Int)] -> Bool
evalPoss prog = case loopF [] $ run (length prog) (0,0) prog of
  Just _  -> False
  Nothing -> True

trueEnding :: [[(String, Int)]] -> Int
trueEnding progs = case find (evalPoss) progs of
  Just prog -> fst $ last $ run (length prog) (0,0) prog
  Nothing   -> error "trueEnding error"

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . trueEnding . possGen [] . map parse . lines   
    >> exitSuccess
--