import qualified Data.Matrix as M
import qualified Data.List as L

data Pawn = White | Black | Blank | OutOfBoard deriving (Eq)

data GameBoard = GameBoard {board :: (M.Matrix Pawn)}

-------------------------------------------------------------------------------

instance Show Pawn where
  show (White) = show 'o'
  show (Black) = show 'x'
  show (Blank) = show " "

instance Show GameBoard where
  show (GameBoard b) = unlines ( map ((take 1 z)++) (lines (show b))) ++ "    " ++ L.intercalate "   " [[c] | c <- z]

-------------------------------------------------------------------------------

initializeBoard = GameBoard $ M.matrix size size (\(i,j) -> Blank)

putPawn col coords b = GameBoard $ M.setElem col coords (board b)

-- np. putPawn White (3,5) $ putPawn Black (3,6) $ putPawn White (2,5) initializeBoard

z = take size ['A'..]
size = 15

--wolne pola
freeList (GameBoard b) = [(x,y)| x <-[1..size], y <- [1..size], (M.getElem x y b) == Blank]

-------------------------------------------------------------------------------

boardRate :: GameBoard -> Pawn -> Int
boardRate (GameBoard b) player = sum $ findNearPositions player (findPlayerPositions b player)

findPlayerPositions :: Eq a => M.Matrix a -> a -> [(Int, Int)]
findPlayerPositions b player = [(x,y)| x <-[1..size], y <- [1..size], (M.getElem x y b) == player]

findNearPositions :: Num b => t -> [(b, b)] -> [b]
findNearPositions player coords = map (ratePoint player) coords

ratePoint :: Num a => t -> (a, a) -> a
ratePoint player coords = (fst coords) + (snd coords)

-------------------------------------------------------------------------------

newtype GameTree = Tree GameBoard deriving (Show)

--possibleMoves [] _ = []
--possibleMoves (move:moves) previousBoard = putPawn ('o' move --previousBoard) ++ possibleMoves (moves previousBoard)
