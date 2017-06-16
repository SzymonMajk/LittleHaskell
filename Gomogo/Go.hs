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

--instance Show GameBoard where
--  show (GameBoard b) = unlines ( map ((take 1 z)++) (lines (show b))) ++ --"    " ++ L.intercalate "   " [[c] | c <- z]

instance Show GameBoard where
  show (GameBoard b) = unlines ( con (wrap size z) (lines (show b))) ++ "    " ++ L.intercalate "   " [[c] | c <- z]

con :: [String] -> [String] -> [String]
con [] _ = []
con _ [] = []
con (x:xs) (y:ys) = (x ++ y):(con xs ys)

wrap :: Int -> String -> [String]
wrap 0 _ = [[]]
wrap depth (x:xs) = [x]:(wrap (depth-1) xs)

z = take size ['A'..]
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
    near x y coords = abs ((fst coords) - x) <= 1 && abs ((snd coords) - y) <= 1

--test findPositionsNearPawn (putPawn White (3,5) ( putPawn Black (3,6) ( putPawn White (2,5) initializeBoard))) $ allBusyPositions $ putPawn White (3,5) $ putPawn Black (3,6) $ putPawn White (2,5) initializeBoard

{-
findNearBlankPositions :: M.Matrix Pawn -> [(Int, Int)] -> [(Int, Int)]
findNearBlankPositions _ [] = []
findNearBlankPositions b (c:cs) = (findNearBlankPosition b c) ++ (findNearBlankPositions b cs)

findNearBlankPosition :: M.Matrix Pawn -> (Int, Int) -> [(Int, Int)]
findNearBlankPosition b coords = [(x,y)| x <-[1..size], y <- [1..size], abs ((fst coords) - x) <= 1 && abs ((snd coords) - y) <= 1 && (fst coords /= x || snd coords /= y) ]
-}

----------------------------  GameBoard rating  -------------------------------

boardRate :: GameBoard -> Int
boardRate b = (playerRate b White) - (playerRate b Black)

playerRate :: GameBoard -> Pawn -> Int
playerRate b player = sum (ratePositions b player (allPlayerPositions b player))

ratePositions :: GameBoard -> Pawn -> [(Int, Int)] -> [Int]
ratePositions b player coords = map (ratePoint b player) coords

ratePoint :: GameBoard -> Pawn -> (Int, Int) -> Int
ratePoint b player coords = 1

----------------------------- GameTree and MinMax -----------------------------

--In this implementation computer cannot start if computer starts it needs to put first Pawn with different algorithm
generateGameTree gameBoard pawn = T.Node gameBoard [generateGameTree (putPawn pawn x gameBoard) (enemyPawn pawn) | x <- findPositionsNearPawn gameBoard (allBusyPositions gameBoard)]
