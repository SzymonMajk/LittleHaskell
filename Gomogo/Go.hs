import qualified Data.Matrix as M
import qualified Data.List as L
import qualified Data.Tree as T

import Data.Maybe (fromJust)
import System.IO
import Data.Char

------------------------ Pawn and GameBoard structures ------------------------
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
----------------------------- GameBoard functions -----------------------------

initializeBoard :: GameBoard
initializeBoard = putPawn Black (mid,mid) (GameBoard $ M.matrix size size (\(i,j) -> Blank)) where
  mid = div (1 + size) 2

putPawn :: Pawn -> (Int, Int) -> GameBoard -> GameBoard
putPawn p coords b = GameBoard $ M.setElem p coords (board b)

getPawn :: GameBoard -> (Int, Int) -> Pawn
getPawn b coords = M.getElem (fst coords) (snd coords) (board b)

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

-------------------------------------------------------------------------------
------------------------------- Win Condition ---------------------------------

data GameCondition = InProgress | UserWin | ComputerWin | Draw deriving Eq

endGame :: GameBoard -> GameCondition
endGame b
  | draw b = Draw
  | lose b Black = UserWin
  | lose b White = ComputerWin
  | win b White = UserWin
  | win b Black = ComputerWin
  | otherwise = InProgress

win :: GameBoard -> Pawn -> Bool
win b player = checkPoints 5 b player (allPlayerPositions b player)

lose :: GameBoard -> Pawn -> Bool
lose b player = checkPoints 6 b player (allPlayerPositions b player)

checkPoints :: Int -> GameBoard -> Pawn -> [(Int, Int)] -> Bool
checkPoints len b player coords = or (map (checkPoint len b player) coords)

checkPoint :: Int -> GameBoard -> Pawn -> (Int, Int) -> Bool
checkPoint len b player coords = or (fmap (checkLine b player coords len) offsets) where
  offsets = [(x,y)| x<-[-1,0,1], y<-[-1,0,1], x /= 0 || y /= 0]

checkLine :: GameBoard -> Pawn -> (Int,Int) -> Int -> (Int,Int) -> Bool
checkLine b player coords len offset
  | len == 0 = True
  | (fst coords) < 1 || (fst coords) > size || (snd coords) < 1 || (snd coords) > size = False
  | player == (getPawn b coords) = checkLine b player (fst coords + fst offset,snd coords + snd offset) (len-1) offset
  | otherwise = False

draw :: GameBoard -> Bool
draw b = length (allFreePositions b) == 0

-------------------------------------------------------------------------------
----------------------------  GameBoard rating  -------------------------------

rateBoard :: GameBoard -> Int
rateBoard b
  | endGame b == UserWin = -100000000
  | endGame b == ComputerWin = 100000000
  | otherwise = (ratePlayer b White) - (ratePlayer b Black)

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
  | (abs (fst offset)) == 1 || (abs (snd offset)) == 1 = check b player ((fst coords)+(fst offset),(snd coords) + (snd offset)) 1
  | otherwise = 0 where
    check b player coords strength
      | getPawn b coords == player = 10*strength + 10
      | getPawn b coords == (enemyPawn player) = -10
      | otherwise = 10

-------------------------------------------------------------------------------
---------------------------- GameTree and MiniMax -----------------------------

generateGameTree :: GameBoard -> Pawn -> Int -> T.Tree GameBoard
generateGameTree gameBoard _ 0 = T.Node gameBoard [] 
generateGameTree gameBoard pawn depth = T.Node gameBoard [generateGameTree (putPawn pawn x gameBoard) (enemyPawn pawn) (depth-1) | x <- findPositionsNearPawn gameBoard (allBusyPositions gameBoard)]

--minimax (T.Node board subTrees) Pawn max = ...

minimaxPutPawn b = (putPawn Black bestMove b) where
  bestMove = head (findPositionsNearPawn b (allBusyPositions b))




-------------------------------------------------------------------------------
------------------------------- User Interface -------------------------------

parseCoordinate :: Char -> (Maybe Int)
parseCoordinate e = if (e < 'A' || e > (chr ((ord 'A') + size - 1))) then Nothing else Just (ord e - 64)

play :: GameBoard -> (IO ())
play board = do
  case (endGame board) of
    UserWin -> (hPutStrLn stderr ((show board) ++ "\nUser Won!"))
    ComputerWin -> (hPutStrLn stderr ((show board) ++ "\nComputer Won!"))
    Draw -> (hPutStrLn stderr ((show board) ++ "\nDraw!"))
    InProgress -> do
      hPutStrLn stderr ("Your turn!\n" ++ (show board))
      putStrLn (show board)
      putStrLn "\nEnter row index: "
      row <- getLine
      if parseCoordinate (head row) == Nothing then do
        putStrLn "Invalid row coordinate!"
        play board
      else do
        putStrLn "\nEnter row column: "
        col <- getLine
        if parseCoordinate (head col) == Nothing then do
          putStrLn "Invalid col coordinate!"
          play board
        else do
          let userMove = (putPawn White (fromJust (parseCoordinate (head row)),fromJust (parseCoordinate (head col))) board)
          case (endGame userMove) of
            UserWin -> (hPutStrLn stderr ((show userMove) ++ "\nUser Won!"))
            ComputerWin -> (hPutStrLn stderr ((show userMove) ++ "\nComputer Won!"))
            Draw -> (hPutStrLn stderr ((show userMove) ++ "\nDraw!"))
            InProgress -> do
              let enemyMove = (minimaxPutPawn userMove)
              putStrLn ("\nEnemy turn!\n" ++ (show enemyMove))
              play enemyMove

-------------------------------------------------------------------------------
------------------------------------ Tests ------------------------------------

--putPawn White (3,5) $ putPawn Black (3,6) $ putPawn White (2,5) initializeBoard

--findPositionsNearPawn (putPawn White (3,5) ( putPawn Black (3,6) ( putPawn White (2,5) initializeBoard))) $ allBusyPositions $ putPawn White (3,5) $ putPawn Black (3,6) $ putPawn White (2,5) initializeBoard

--endGame $ putPawn White (3,5) $ putPawn Black (3,6) $ putPawn White (2,5) initializeBoard
--endGame $ putPawn White (3,1) $ putPawn White (3,4) $ putPawn White (3,3) $ putPawn White (3,2) $ putPawn White (3,5) $ putPawn Black (3,6) $ putPawn White (2,5) initializeBoard
--endGame $ putPawn White (3,1) $ putPawn White (3,4) $ putPawn White (3,3) $ putPawn White (3,2) $ putPawn White (3,5) $ putPawn White (3,6) $ putPawn White (2,5) initializeBoard

--test rateBoard (putPawn White (3,5) $ putPawn Black (3,6) $ putPawn White (2,5) initializeBoard)
