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

z = take size ['A'..]
size = 15

-------------------------------------------------------------------------------

initializeBoard :: GameBoard
initializeBoard = GameBoard $ M.matrix size size (\(i,j) -> Blank)

putPawn :: Pawn -> (Int, Int) -> GameBoard -> GameBoard
putPawn col coords b = GameBoard $ M.setElem col coords (board b)

-- np. putPawn White (3,5) $ putPawn Black (3,6) $ putPawn White (2,5) initializeBoard

allFreePositions :: GameBoard -> [(Int, Int)]
allFreePositions (GameBoard b) = [(x,y)| x <-[1..size], y <- [1..size], (M.getElem x y b) == Blank]

-------------------------------------------------------------------------------

--boardRate (GameBoard b) player = sum (ratePositions player (takeBlankPositions b (findNearPositions (findPlayerPositions b player))))
boardRate :: Num a => GameBoard -> Pawn -> a
boardRate (GameBoard b) player = sum (ratePositions player (findNearBlankPositions b (findPlayerPositions b player)))

findPlayerPositions :: Eq a => M.Matrix a -> a -> [(Int, Int)]
findPlayerPositions b player = [(x,y)| x <-[1..size], y <- [1..size], (M.getElem x y b) == player]

--findNearPositions [] = []
--findNearPositions (c:cs) = (findNearPosition c) ++ (findNearPositions cs)
--findNearPosition coords = [(x,y)| x <-[1..size], y <- [1..size], abs ((fst coords) - x) <= 1 && abs ((snd coords) - y) <= 1 && (fst coords /= x || snd coords /= y)]

findNearBlankPositions :: M.Matrix Pawn -> [(Int, Int)] -> [(Int, Int)]
findNearBlankPositions _ [] = []
findNearBlankPositions b (c:cs) = (findNearBlankPosition b c) ++ (findNearBlankPositions b cs)

findNearBlankPosition :: M.Matrix Pawn -> (Int, Int) -> [(Int, Int)]
findNearBlankPosition b coords = [(x,y)| x <-[1..size], y <- [1..size], abs ((fst coords) - x) <= 1 && abs ((snd coords) - y) <= 1 && (fst coords /= x || snd coords /= y) && (M.getElem x y b) == Blank]

ratePositions :: Num a => t -> [t1] -> [a]
ratePositions player coords = map (ratePoint player) coords

ratePoint :: Num a => t -> t1 -> a
ratePoint player coords = 1

-------------------------------------------------------------------------------

newtype GameTree = Tree GameBoard deriving (Show)

--putStrLn $ drawTree $ fmap show $ met 4
