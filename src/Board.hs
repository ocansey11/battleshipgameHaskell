module Board
    ( Board(..)
    , CellState(..)
    , createEmptyBoard
    , placeShip
    , placeShipsRandomly
    , makeMove
    , isValidPosition
    , canPlaceShip
    , isShipSunk
    , allShipsSunk
    , boardSize
    ) where

import Ship
import System.Random
import qualified Data.Map as Map
import Data.Map (Map)

-- | Size of the game board
boardSize :: Int
boardSize = 9

-- | State of a cell on the board
data CellState = Empty | HasShip ShipType | Hit ShipType | Miss
    deriving (Show, Eq)

-- | Game board representation
data Board = Board
    { cells :: Map Position CellState
    , ships :: [Ship]
    } deriving (Show)

-- | Create an empty board
createEmptyBoard :: Board
createEmptyBoard = Board
    { cells = Map.fromList [(Position r c, Empty) | r <- [0..boardSize-1], c <- [0..boardSize-1]]
    , ships = []
    }

-- | Check if a position is valid (within board bounds)
isValidPosition :: Position -> Bool
isValidPosition (Position row col) = 
    row >= 0 && row < boardSize && col >= 0 && col < boardSize

-- | Check if a ship can be placed at the given position
canPlaceShip :: Board -> Ship -> Bool
canPlaceShip board ship =
    let positions = shipPositions ship
    in all isValidPosition positions && 
       all (\pos -> Map.lookup pos (cells board) == Just Empty) positions

-- | Place a ship on the board
placeShip :: Board -> Ship -> Either String Board
placeShip board ship
    | not (canPlaceShip board ship) = Left "Cannot place ship at this position"
    | otherwise = Right $ Board
        { cells = foldr (\pos acc -> Map.insert pos (HasShip (shipType ship)) acc) (cells board) (shipPositions ship)
        , ships = ship : ships board
        }

-- | Generate random ships and place them on the board
placeShipsRandomly :: StdGen -> Board -> (Board, StdGen)
placeShipsRandomly gen board =
    let shipTypes = [Carrier, Battleship, Cruiser, Submarine, Destroyer]
    in placeShipsRandomly' gen board shipTypes
  where
    placeShipsRandomly' g b [] = (b, g)
    placeShipsRandomly' g b (sType:rest) =
        let (newBoard, newGen) = placeShipRandomly g b sType
        in placeShipsRandomly' newGen newBoard rest

-- | Place a single ship randomly on the board
placeShipRandomly :: StdGen -> Board -> ShipType -> (Board, StdGen)
placeShipRandomly gen board shipType = tryPlacement gen
  where
    tryPlacement g =
        let (row, g1) = randomR (0, boardSize - 1) g
            (col, g2) = randomR (0, boardSize - 1) g1
            (orientInt, g3) = randomR (0, 1 :: Int) g2
            orient = if orientInt == 0 then Horizontal else Vertical
            ship = createShip shipType (Position row col) orient
        in case placeShip board ship of
            Right newBoard -> (newBoard, g3)
            Left _ -> tryPlacement g3

-- | Make a move on the board (attack a position)
makeMove :: Position -> Board -> (Board, Bool, Bool) -- (new board, hit?, sunk?)
makeMove pos board =
    case Map.lookup pos (cells board) of
        Nothing -> (board, False, False) -- Invalid position
        Just Empty -> 
            let newCells = Map.insert pos Miss (cells board)
            in (board { cells = newCells }, False, False)
        Just Miss -> (board, False, False) -- Already tried
        Just (Hit _) -> (board, False, False) -- Already hit
        Just (HasShip sType) ->
            let newCells = Map.insert pos (Hit sType) (cells board)
                newBoard = board { cells = newCells }
                hitShip = findShipAtPosition pos (ships board)
                sunk = maybe False (isShipSunk newBoard) hitShip
            in (newBoard, True, sunk)

-- | Find the ship at a given position
findShipAtPosition :: Position -> [Ship] -> Maybe Ship
findShipAtPosition pos = foldr (\ship acc -> if pos `elem` shipPositions ship then Just ship else acc) Nothing

-- | Check if a ship is completely sunk
isShipSunk :: Board -> Ship -> Bool
isShipSunk board ship =
    let positions = shipPositions ship
    in all (\pos -> case Map.lookup pos (cells board) of
                Just (Hit _) -> True
                _ -> False) positions

-- | Check if all ships are sunk
allShipsSunk :: Board -> Bool
allShipsSunk board = all (isShipSunk board) (ships board)
