import qualified Data.Matrix as M
import qualified Data.List as L
import qualified Data.Tree as T

data Pawn = White | Black | Blank deriving (Eq)

instance Show Pawn where
  show (White) = show 'o'
  show (Black) = show 'x'
  show (Blank) = show ' '

enemyPawn White = Black
enemyPawn Black = White

data GameBoard = GameBoard {board :: (M.Matrix Pawn)}

instance Show GameBoard where
  show (GameBoard b) = unlines (con (wrapColumn size leftColumn) (lines (show b))) ++ "    " ++ L.intercalate "   " [[c] | c <- leftColumn]

con :: [String] -> [String] -> [String]
con [] _ = []
con _ [] = []
con (x:xs) (y:ys) = (x ++ y):(con xs ys)

wrapColumn :: Int -> String -> [String]
wrapColumn 0 _ = [[]]
wrapColumn depth (x:xs) = [x]:(wrapColumn (depth-1) xs)

leftColumn = take size ['A'..]
size = 19

----------------------------- GameBoard functions -----------------------------

initializeBoard :: GameBoard
initializeBoard = GameBoard $ M.matrix size size (\(i,j) -> Blank)

putPawn :: Pawn -> (Int, Int) -> GameBoard -> GameBoard
putPawn p coords b = GameBoard $ M.setElem p coords (board b)

getPawn :: GameBoard -> (Int, Int) -> Pawn
getPawn b coords = M.getElem (fst coords) (snd coords) (board b)

--test putPawn White (3,5) $ putPawn Black (3,6) $ putPawn White (2,5) initializeBoard

allFreePositions :: GameBoard -> [(Int, Int)]
allFreePositions (GameBoard b) = [(x,y)| x <-[1..size], y <- [1..size], (M.getElem x y b) == Blank]

allBusyPositions :: GameBoard -> [(Int, Int)]
allBusyPositions (GameBoard b) = [(x,y)| x <-[1..size], y <- [1..size], (M.getElem x y b) /= Blank]

allPlayerPositions :: GameBoard -> Pawn -> [(Int, Int)]
allPlayerPositions (GameBoard b) player = [(x,y)| x <-[1..size], y <- [1..size], (M.getElem x y b) == player]

findPositionsNearPawn :: GameBoard -> [(Int, Int)] -> [(Int, Int)]
findPositionsNearPawn b [] = []
findPositionsNearPawn b (c:cs) = (searchBlankNear b c) ++ (findPositionsNearPawn b cs) where
  searchBlankNear b coords = [(x,y)| x <-[1..size], y <- [1..size], (near x y coords) && (getPawn b (x,y)) == Blank]
  near x y coords = abs ((fst coords)-x) <= 1 && abs ((snd coords)-y) <= 1

--test findPositionsNearPawn (putPawn White (3,5) ( putPawn Black (3,6) ( putPawn White (2,5) initializeBoard))) $ allBusyPositions $ putPawn White (3,5) $ putPawn Black (3,6) $ putPawn White (2,5) initializeBoard

----------------------------  GameBoard rating  -------------------------------

rateBoard :: GameBoard -> Int
rateBoard b = (ratePlayer b White) - (ratePlayer b Black)

ratePlayer :: GameBoard -> Pawn -> Int
ratePlayer b player = sum (ratePositions b player (allPlayerPositions b player))

ratePositions :: GameBoard -> Pawn -> [(Int, Int)] -> [Int]
ratePositions b player coords = map (ratePoint b player) coords

ratePoint :: GameBoard -> Pawn -> (Int, Int) -> Int
ratePoint b player coords = foldr (+) 0 (fmap (rateAmbience b player coords) offsets) where
  offsets = [(x,y)| x <- [-4..4], y <- [-4..4], ((abs x) - (abs y)) == 0 || 0 == x || 0 == y]

rateAmbience :: GameBoard -> Pawn -> (Int, Int) -> (Int, Int) -> Int
rateAmbience b player coords offset
  | (fst coords) + (fst offset) < 1 || (fst coords) + (fst offset) >= size = 0
  | (snd coords) + (snd offset) < 1 || (snd coords) + (snd offset) >= size = 0
  | (abs (fst offset)) == 4 || (abs (snd offset)) == 4 = check b player ((fst coords)+(fst offset),(snd coords) + (snd offset)) 4
  | (abs (fst offset)) == 3 || (abs (snd offset)) == 3 = check b player ((fst coords)+(fst offset),(snd coords) + (snd offset)) 3
  | (abs (fst offset)) == 2 || (abs (snd offset)) == 2 = check b player ((fst coords)+(fst offset),(snd coords) + (snd offset)) 2
  | (abs (fst offset)) == 1 || (abs (snd offset)) == 1 = check b player ((fst coords)+(fst offset),(snd coords) + (snd offset))
  | otherwise = 0 where
    check b player coords strength
      | getPawn b coords == player = 10*strength + 10
      | getPawn b coords == (enemyPawn player) = -10
      | otherwise = 10

--test rateBoard (putPawn White (3,5) $ putPawn Black (3,6) $ putPawn White (2,5) initializeBoard)

----------------------------- GameTree and MinMax -----------------------------

--In this implementation computer cannot start if computer starts it needs to put first Pawn with different algorithm
generateGameTree gameBoard pawn = T.Node gameBoard [generateGameTree (putPawn pawn x gameBoard) (enemyPawn pawn) | x <- findPositionsNearPawn gameBoard (allBusyPositions gameBoard)]


