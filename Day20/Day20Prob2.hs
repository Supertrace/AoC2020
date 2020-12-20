module Main where

import System.Environment
import System.Exit
import System.IO
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

type Board = M.HashMap (Int, Int) Char

data Tile = Tile Int Board
  deriving (Eq,Show)

instance Ord Tile where
  Tile n1 _ <= Tile n2 _ = n1 <= n2

type TileArr = M.HashMap (Int,Int) Tile

type Pattern = (Board, Int, Int)

grouper :: [String] -> [[String]]
grouper []  = []
grouper lns = (group) : (grouper rem) where 
                lsts  = break (== []) lns
                group = fst lsts
                rem   = drop 1 $ snd lsts

tileGen :: [[a]] -> M.HashMap (Int, Int) a
tileGen strs = M.fromList $ map (\(c,x,y) -> ((x,y),c)) $ concat $ map yCoord coordList where
  coordList = flip zip [0..] $ map (flip zip [0..]) strs
  yCoord :: ([(a,Int)], Int) -> [(a,Int,Int)]
  yCoord ([],_) = []
  yCoord ((char, x):rest, y) = (char, x, y) : yCoord (rest, y)

tileParse :: [String] -> Tile
tileParse (title:grid) = Tile num $ tileGen grid where
  num = read $ reverse $ drop 1 $ reverse $ drop 1 $ dropWhile (/= ' ') title

rotate :: Int -> Board -> Board
rotate dim mep = M.mapWithKey (\(x,y) _ -> mep M.! (dim - y, x)) mep

flip8 :: Int -> Board -> Board
flip8 dim mep = M.mapWithKey (\(x,y) _ -> mep M.! (dim - x, y)) mep

d8 :: [[Int -> Board -> Board]]
d8 = [[(\_ -> id)], [flip8], [rotate], [rotate,flip8], [rotate,rotate], [rotate,rotate,flip8], [rotate,rotate,rotate], [rotate,rotate,rotate,flip8]]

transD8 :: Int -> [Board -> Board]
transD8 dim = map (foldr1 (.)) $ map (map (flip ($) dim)) d8

bToTile :: (Board -> Board) -> Tile -> Tile
bToTile f (Tile iD board) = Tile iD $ f board

sMatch :: String -> Tile -> Bool
sMatch side tile = side `elem` ls where
  ls' = getTileLines tile
  ls  = ls' ++ (map reverse ls')

whichSMatch :: Tile -> Tile -> [(String, Bool)]
whichSMatch tile1 tile2 = zip sides tVals where
  sides = getTileLines tile1
  tVals = map (flip sMatch tile2) sides

sideMatches :: [Tile] -> Tile -> Int
sideMatches tiles tile = result where
  sides  = getTileLines tile
  result = length $ filter ((> 1) . length) $ map (\side -> filter (sMatch side) tiles) sides

getTileLines :: Tile -> [String]
getTileLines (Tile _ mep) = [top, rside, bottom, lside] where
  top    = map (mep M.!) [(x,0) | x <- [0..9]]
  lside  = map (mep M.!) [(0,y) | y <- [0..9]]
  rside  = map (mep M.!) [(9,y) | y <- [0..9]]
  bottom = map (mep M.!) [(x,9) | x <- [0..9]]

findPath :: S.Set Tile -> [Tile] -> Tile -> Tile -> Maybe [Tile]
findPath _     p _ _ | length p > 12                       = Nothing
findPath _     p (Tile id1 _) e@(Tile id2 _)  | id1 == id2 = Just $ reverse (e:p)
findPath sideT p s e = case S.filter (any snd . whichSMatch s) sideT of
  s' | S.size s' > 0 -> tryAll s' sideT (s:p) e
  _                  -> Nothing

tryAll :: S.Set Tile -> S.Set Tile -> [Tile] -> Tile -> Maybe [Tile]
tryAll choices _     _ _ | S.size choices == 0 = Nothing
tryAll choices sideT p e = result where
  s        = S.findMin choices
  newC     = S.deleteMin choices
  newSides = S.delete s sideT
  result   = case findPath newSides p s e of
               Nothing -> tryAll newC sideT p e
               a       -> a

ident :: Tile -> Int
ident (Tile num _) = num

unOrdPair :: [a]-> [(a,a)]
unOrdPair [] = []
unOrdPair (x:lst) = zip (iterate id x) lst ++ unOrdPair lst

cycler :: [Int] -> [(Int, Int)] -> [Int]
cycler soFar@(x:rest) rules = result where
  checker a = (not $ a `elem` soFar) && ((x,a) `elem` rules || (a,x) `elem` rules)
  result = case find checker $ (map fst rules) ++ (map snd rules) of
             Just elt -> cycler (elt:soFar) rules
             _        -> reverse soFar

cornerList :: [Tile] -> [(Tile,Tile)] -> [Tile]
cornerList (tile:lst) rules = tile : (catMaybes [find ((== num) . ident) lst | num <- cycle]) where
  tId       = ident tile
  ruleIdLst = map (\(a,b) -> (ident a, ident b)) rules
  cycle     = cycler [tId] ruleIdLst

sideF :: (Tile, Tile) -> [(Tile,Tile,[Tile])] -> [Tile]
sideF (a,b) sides = case find (\(c,d,_) -> (c,d)==(a,b)) sides of
  Just s  -> (\(_,_,side) -> side) s
  Nothing -> reverse $ (\(_,_,side) -> side) $ fromJust $ find (\(c,d,_) -> (d,c) == (a,b)) sides

orient :: TileArr -> TileArr -> (Int, Int) -> Maybe TileArr
orient orig soFar loc    = result where
  incr :: (Int, Int) -> (Int, Int)
  incr (x,y) = case x < 11 of
                 True -> (x + 1, y)
                 _    -> (0, y+1)

  upCheck :: TileArr -> (Int, Int) -> Bool
  upCheck arr l@(x,y) = case  y == 0 of
                          True  -> True
                          False -> top == bot where
                                     top = head $ getTileLines $ arr M.! l
                                     bot = head $ drop 2 $ getTileLines $ arr M.! (x,y-1)

  leftCheck :: TileArr -> (Int, Int) -> Bool
  leftCheck arr l@(x,y) = case  x == 0 of
                            True  -> True
                            False -> left == right where
                                       left  = head $ drop 3 $ getTileLines $ arr M.! l
                                       right = head $ drop 1 $ getTileLines $ arr M.! (x-1,y)


  result = case all (flip ($) (soFar, loc) . uncurry) [upCheck, leftCheck] of
             True -> tryAllO (map bToTile $ transD8 9) orig soFar $ incr loc
             _    -> Nothing

tryAllO :: [Tile -> Tile] -> TileArr -> TileArr -> (Int, Int) -> Maybe TileArr
tryAllO []        _    _     _      = Nothing
tryAllO _         _    soFar (_,12) = Just soFar
tryAllO (f:trans) orig soFar loc    = result where
  newFar = M.insert loc (f $ orig M.! loc) soFar
  result = case orient orig newFar loc of
             Nothing -> tryAllO trans orig soFar loc
             a       -> a

bigTile :: TileArr -> (Int, Int, Int, Int) -> Board -> Board
bigTile template loc@(bX,bY,x,y) soFar = case bY < 12 of
  True -> bigTile template (incr loc) newBoard where
            minbX = 0
            maxbX = 11
            minxy = 1
            maxxy = 8
            incr :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
            incr (bX', bY', x', y') = case x' < maxxy of
                                        True -> (bX', bY', x' + 1, y')
                                        _    -> case y' < maxxy of
                                                  True -> (bX', bY', minxy, y' + 1)
                                                  _    -> case bX' < maxbX of
                                                            True -> (bX' + 1, bY', minxy, minxy)
                                                            _    -> (minbX, bY' + 1, minxy, minxy)
            smallDim   = (maxxy - minxy) + 1
            Tile _ smallArray = template M.! (bX,bY)
            newBoard = M.insert (bX * smallDim + x - minxy, bY*smallDim + y - minxy) (smallArray M.! (x,y)) soFar
  _    -> soFar

seaMonster :: Pattern
seaMonster = (tileGen patt, length $ head patt, length patt) where
  patt = ["                  # ","#    ##    ##    ###"," #  #  #  #  #  #   "]

patternMatch :: Pattern -> Board -> (Int, Int) -> Bool
patternMatch (patTile, xDim, yDim) grid (x,y) = all charCheck coords where
  xShifts = [0..xDim-1]
  yShifts = [0..yDim-1]
  coords  = [(x + x', y + y',x',y') | x' <- xShifts, y' <- yShifts]
  charCheck :: (Int, Int, Int ,Int) -> Bool
  charCheck (x1,y1,x2,y2) = case patTile M.! (x2,y2) of
    '#' -> grid M.! (x1,y1) == '#'
    ' ' -> True

patternFind :: Pattern -> Int -> Board  -> Bool
patternFind pat@(_, xDim, yDim) maxXY grid = any (patternMatch pat grid) coords where
  xRange = [0..(maxXY-xDim)+1]
  yRange = [0..(maxXY-yDim)+1]
  coords = [(x,y) | x <- xRange, y <- yRange]

monsterFind :: Board -> Int -> Board
monsterFind board maxXY = head $ filter (patternFind seaMonster maxXY) possBoards where
  possBoards = map ($ board) $ transD8 maxXY

seaMonsterToLook :: Pattern -> [(Int, Int)]
seaMonsterToLook patt = map fst $ filter ((== '#') . snd) $ M.toList mep where
  (mep, _, _) = patt

seaMonsterLocs :: Board -> Int -> Pattern -> S.Set (Int, Int)
seaMonsterLocs board maxXY pat@(_,xDim,yDim) = S.unions $ map (flip shifter pattLocs) patHits where
  xRange   = [0..(maxXY-xDim)+1]
  yRange   = [0..(maxXY-yDim)+1]
  coords   = [(x,y) | x <- xRange, y <- yRange]
  pattLocs = seaMonsterToLook pat
  patHits  = filter (patternMatch pat board) coords
  shifter :: (Int, Int) -> [(Int, Int)] -> S.Set (Int, Int)
  shifter (x,y) lst = S.fromList $ map (\(x',y') -> (x+x',y+y')) lst

allHashes :: Board -> S.Set (Int, Int)
allHashes board = S.fromList $ map fst $ filter ((== '#') . snd) $ M.toList board

--helper :: [[String]] -> Int
helper inputs = roughness where
  tiles        = map tileParse inputs
  tilesWSides  = zip tiles $ map (sideMatches tiles) tiles
  corner       = map fst $ filter ((== 2) . snd) tilesWSides
  cornerS      = S.fromList corner
  side         = S.fromList $ map fst $ filter ((== 3) . snd) tilesWSides
  inside       = S.fromList $ map fst $ filter ((== 4) . snd) tilesWSides
  outsidePaths = filter (\(_,_,a) -> isJust a) [(c,c',findPath (S.insert c' side) [] c c')| (c,c') <- unOrdPair corner]
  oPaths       = map (\(a,b,c) -> (a,b, fromJust c)) outsidePaths
  ordCorn      = cornerList corner $ map (\(a,b,_) -> (a,b)) oPaths
  [a,b,c,d]    = ordCorn
  topS         = sideF (a,b) oPaths
  rightS       = sideF (b,c) oPaths
  leftS        = sideF (a,d) oPaths
  botS         = sideF (d,c) oPaths
  sidePair     = reverse $ drop 1 $ reverse $ drop 1 $ zip leftS rightS
  inPaths      = catMaybes [findPath (S.insert s' inside) [] s s'| (s,s') <- sidePair]
  bMat         = tileGen $ (topS : (inPaths ++ [botS]))
  oriented     = fromJust $ tryAllO (map bToTile $ transD8 9) bMat M.empty (0,0)
  newBoard     = bigTile oriented (0,0,1,1) M.empty
  oNewBoard    = monsterFind newBoard 95
  monsterSpots = seaMonsterLocs oNewBoard 95 seaMonster
  roughness    = S.size $ (allHashes oNewBoard) S.\\ monsterSpots

main :: IO()
main = do
    args <- getArgs
    (readFile $ head args) >>= putStrLn . show . helper . grouper . lines   
    >> exitSuccess
--