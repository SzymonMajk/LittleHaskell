import qualified Data.Matrix as M
import qualified Data.List as L

data Pawn = Color Char | Blank | OutOfBoard deriving (Eq)

data GameBoard = GameBoard {board :: (M.Matrix Pawn)}

instance Show Pawn where
  show (Color c) = show c
  show (Blank) = show " "

instance Show GameBoard where
  show (GameBoard b) = unlines ( map ((take 1 z)++) (lines (show b))) ++ "    " ++ L.intercalate "   " [[c] | c <- z]

initializeBoard = GameBoard $ M.matrix size size (\(i,j) -> Blank)

putPawn c k b = GameBoard $ M.setElem (Color c) k (board b)

-- np. putPawn 'o' (3,5) $ putPawn 'x' (3,6) $ putPawn 'o' (2,5) initializeBoard

z = take size ['A'..]
size = 15

--wolne pola
freeList (GameBoard b) = [(x,y)| x <-[1..size], y <- [1..size], (M.getElem x y b) == Blank]

--ocena planszy
boardRating board player = undefined


newtype GameTree = Tree GameBoard deriving (Show)

--possibleMoves [] _ = []
--possibleMoves (move:moves) previousBoard = putPawn ('o' move --previousBoard) ++ possibleMoves (moves previousBoard)
