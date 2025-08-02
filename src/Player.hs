module Player
    ( Player(..)
    , PlayerType(..)
    , createPlayer
    , makeComputerMove
    , getAvailableMoves
    ) where

import Ship
import Board
import System.Random
import qualified Data.Set as Set
import Data.Set (Set)

-- | Type of player
data PlayerType = Human | Computer
    deriving (Show, Eq)

-- | Player data structure
data Player = Player
    { playerType :: PlayerType
    , playerBoard :: Board
    , movesMade :: Set Position  -- Positions already attacked on opponent's board
    } deriving (Show)

-- | Create a new player
createPlayer :: PlayerType -> Board -> Player
createPlayer pType board = Player pType board Set.empty

-- | Get all available moves for a player
getAvailableMoves :: Player -> [Position]
getAvailableMoves player =
    let allPositions = [Position r c | r <- [0..boardSize-1], c <- [0..boardSize-1]]
    in filter (`Set.notMember` movesMade player) allPositions

-- | Make a computer move (random selection from available moves)
makeComputerMove :: StdGen -> Player -> (Position, StdGen)
makeComputerMove gen player =
    let availableMoves = getAvailableMoves player
    in if null availableMoves
       then error "No available moves"
       else let (index, newGen) = randomR (0, length availableMoves - 1) gen
            in (availableMoves !! index, newGen)
