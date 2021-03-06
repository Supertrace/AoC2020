module Day1Prob1 where
-- Searches through file to find two numbers summing to 2020
-- and then mutliplies them together
-- usage: Day1Prob1 filename

import System.IO
import System.Exit
import System.Environment
import Data.List

nummers :: String -> [Integer]
-- Turns file contents into list of integers
nummers = map read . filter (/= "") . lines

find2020 :: [Integer] -> String
-- Picks out the two numbers that sum to 2020 and mults 'em
find2020 (x : xs) = case find (== 2020) (map (+ x) xs) of
                        Just y  -> show (x * (y-x))
                        Nothing -> find2020 xs
find2020 _        = error "find2020 Matching Error"


main :: IO()
main = do 
        args <- getArgs 
        (readFile $ head args) >>= putStrLn . find2020 . nummers
        >> exitSuccess