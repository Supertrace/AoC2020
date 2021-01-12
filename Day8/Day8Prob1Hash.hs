module Day8Prob1 where

import System.Environment
import System.Exit
import System.IO
import Data.List
import qualified Data.HashMap.Lazy as M
import qualified Data.Set as S

type State = (Int, Int)
type Inst = (String, Int)
type Prog = M.HashMap Int Inst

exec :: Prog -> State -> State
exec prog (acum, loc) = let (s, val) = prog M.! loc in case s of
  "nop" -> (acum, loc + 1)
  "acc" -> (acum + val, loc + 1)
  "jmp" -> (acum, loc + val)

parse :: String -> Inst
parse s = let (a,b) = break (== ' ') s in (a, numEval $ drop 1 b)

numEval :: String -> Int
numEval str@(s:t) = case s of
  '-' -> read str
  _   -> read t

run :: State -> Prog -> [State]
run initState prog = iterate (exec prog) initState

loopF :: S.Set Int -> [State] -> Int
loopF tried ((a,b):s) = case b `S.member` tried of
  True  -> a
  False -> loopF (b `S.insert` tried) s

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . loopF (S.empty) . run (0,0) . M.fromList . zip [0..] . map parse . lines   
    >> exitSuccess
--