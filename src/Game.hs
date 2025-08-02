module Game
    ( GameState(..)
    , GameResult(..)
    , initGame
    , playTurn
    , isGameOver
    , getCurrentPlayer
    , getOpponentPlayer
    , switchPlayer
    ) where

import Ship
import Board
import Player
import Utils
import System.Random
import qualified Data.Set as Set

-- | Current state of the game
data GameState = GameState
    { player1 :: Player
    , player2 :: Player
    , currentPlayerNum :: Int  -- 1 or 2
    , randomGen :: StdGen
    } deriving (Show)

-- | Result of the game
data GameResult = Player1Wins | Player2Wins | GameContinues
    deriving (Show, Eq)

-- | Initialize a new game
initGame :: StdGen -> GameState
initGame gen =
    let (gen1, gen2) = split gen
        (board1, gen1') = placeShipsRandomly gen1 createEmptyBoard
        (board2, gen2') = placeShipsRandomly gen2 createEmptyBoard
        p1 = createPlayer Human board1
        p2 = createPlayer Computer board2
    in GameState p1 p2 1 gen1'

-- | Get the current player
getCurrentPlayer :: GameState -> Player
getCurrentPlayer game = if currentPlayerNum game == 1 then player1 game else player2 game

-- | Get the opponent player
getOpponentPlayer :: GameState -> Player
getOpponentPlayer game = if currentPlayerNum game == 1 then player2 game else player1 game

-- | Switch to the other player
switchPlayer :: GameState -> GameState
switchPlayer game = game { currentPlayerNum = if currentPlayerNum game == 1 then 2 else 1 }

-- | Play a turn
playTurn :: Position -> GameState -> (GameState, Bool, Bool, GameResult)
playTurn pos game =
    let currentP = getCurrentPlayer game
        opponentP = getOpponentPlayer game
        opponentBoard = playerBoard opponentP
        (newOpponentBoard, hit, sunk) = makeMove pos opponentBoard
        newOpponentPlayer = opponentP { playerBoard = newOpponentBoard }
        newCurrentPlayer = currentP { movesMade = Set.insert pos (movesMade currentP) }
        
        -- Update the game state
        updatedGame = if currentPlayerNum game == 1
                     then game { player1 = newCurrentPlayer, player2 = newOpponentPlayer }
                     else game { player1 = newOpponentPlayer, player2 = newCurrentPlayer }
        
        -- Check for game over
        gameResult = if allShipsSunk newOpponentBoard
                    then if currentPlayerNum game == 1 then Player1Wins else Player2Wins
                    else GameContinues
                    
        -- Switch player only if it's a miss
        finalGame = if hit then updatedGame else switchPlayer updatedGame
        
    in (finalGame, hit, sunk, gameResult)

-- | Check if the game is over
isGameOver :: GameState -> Bool
isGameOver game = allShipsSunk (playerBoard (player1 game)) || allShipsSunk (playerBoard (player2 game))
