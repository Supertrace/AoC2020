module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import Data.Maybe
import qualified Data.HashMap.Lazy as M
import qualified Data.Set as S

type State = (Int, Int)
type Inst  = (String, Int)
type Prog  = M.HashMap Int Inst

exec :: Prog -> Maybe State -> Maybe State
exec _    Nothing            = Nothing
exec prog (Just (acum, loc)) = (prog M.!? loc) >>= return . next where
  next (s,val) = case s of
    "nop" -> (acum, loc + 1)
    "acc" -> (acum + val, loc + 1)
    "jmp" -> (acum, loc + val)

parse :: String -> Inst
parse s = let (a,b) = break (== ' ') s in (a, numEval $ drop 1 b)

numEval :: String -> Int
numEval str@(s:t) = case s of
  '-' -> read str
  _   -> read t

run :: State -> Prog -> [Maybe State]
run initState prog = iterate (exec prog) $ Just initState

loopF :: S.Set Int -> [Maybe State] -> Maybe Int
loopF tried (s:rest) = s >>= cont where
  cont (a,b) = case b `S.member` tried of
    True  -> Just a
    False -> loopF (b `S.insert` tried) rest

possGen :: Prog -> [Prog]
possGen initProg = mapMaybe newProg $ M.toList initProg where
  newProg (loc, (inst, val)) = case inst of
    "nop" -> Just $ M.insert loc ("jmp", val) initProg
    "jmp" -> Just $ M.insert loc ("nop", val) initProg
    _     -> Nothing

evalPoss :: Prog -> Bool
evalPoss = isNothing . loopF S.empty . run (0,0)

trueEnding :: [Prog] -> Int
trueEnding progs = case find (evalPoss) progs of
  Just prog -> fst $ last $ map fromJust $ takeWhile (isJust) $ run (0,0) prog
  Nothing   -> error "trueEnding error"

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . trueEnding . possGen . M.fromList . zip [0..] . map parse . lines   
    >> exitSuccess
--