module Main (main) where

import Game
import Player
import Utils
import Ship
import Board
import System.Random
import System.IO
import qualified Data.Set as Set
import qualified Data.Map as Map

main :: IO ()
main = do
    clearScreen
    displayGameTitle
    putStrLn "Welcome to the enhanced Haskell Battleship experience!"
    putStrLn "   Much better than the old JavaScript version!"
    putStrLn ""
    putStrLn "Setting up the battlefield..."
    gen <- getStdGen
    gameLoop (initGame gen)

gameLoop :: GameState -> IO ()
gameLoop game = do
    let currentP = getCurrentPlayer game
        opponentP = getOpponentPlayer game
        playerNum = currentPlayerNum game
    
    -- Display boards with enhanced visuals
    clearScreen
    displayGameTitle
    
    -- Show whose turn it is
    if playerNum == 1
    then putStrLn ">>> YOUR TURN - Choose your target wisely!"
    else putStrLn ">>> COMPUTER'S TURN - Calculating optimal strike..."
    putStrLn ""
    
    -- Display game stats
    let playerShipsRemaining = length $ filter (\ship -> not (isShipSunk (playerBoard currentP) ship)) (ships (playerBoard currentP))
        opponentShipsRemaining = length $ filter (\ship -> not (isShipSunk (playerBoard opponentP) ship)) (ships (playerBoard opponentP))
    displayGameStats playerShipsRemaining opponentShipsRemaining
    
    putStrLn "=== YOUR FLEET ==="
    putStrLn $ displayBoardWithShips (playerBoard currentP)
    putStrLn ""
    putStrLn "=== ENEMY WATERS (Your Attacks) ==="
    putStrLn $ displayBoardForAttacks (playerBoard opponentP) (movesMade currentP)
    putStrLn ""
    
    -- Check if game is over
    if isGameOver game
    then do
        clearScreen
        displayGameTitle
        putStrLn "*** GAME OVER! ***"
        putStrLn ""
        if allShipsSunk (playerBoard (player1 game))
        then do
            putStrLn "The computer has sunk all your ships!"
            putStrLn "*** COMPUTER WINS! ***"
        else do
            putStrLn "You have destroyed the enemy fleet!"
            putStrLn "*** YOU WIN! ***"
        putStrLn ""
        putStrLn "Thanks for playing this enhanced Haskell Battleship!"
        putStrLn "Much more functional than the original JavaScript version!"
    else do
        -- Get move from current player
        pos <- if playerType currentP == Human
               then getHumanMove
               else do
                   let (computerPos, newGen) = makeComputerMove (randomGen game) currentP
                   putStrLn $ "Computer attacks position: " ++ show computerPos
                   putStrLn "Processing attack... Press Enter to continue..."
                   _ <- getLine
                   return computerPos
        
        -- Play the turn
        let (newGame, hit, sunk, result) = playTurn pos game
        
        -- Show result of move with enhanced feedback
        if hit
        then do
            putStrLn "*** DIRECT HIT! ***"
            if sunk then putStrLn ">>> ENEMY SHIP DESTROYED!" else putStrLn ">>> Keep firing at this area!"
        else putStrLn "Splash! You missed this time."
        
        -- Check game result
        case result of
            Player1Wins -> do
                clearScreen
                displayGameTitle
                putStrLn "*** VICTORY! YOU HAVE WON THE BATTLE! ***"
                putStrLn "All enemy ships have been destroyed!"
                putStrLn ""
                putStrLn "Thanks for playing this functional Battleship experience!"
            Player2Wins -> do
                clearScreen
                displayGameTitle
                putStrLn "*** DEFEAT! The computer has won this battle! ***"
                putStrLn "All your ships have been sunk!"
                putStrLn ""
                putStrLn "Better luck next time, Admiral!"
            GameContinues -> do
                putStrLn ""
                putStrLn "Press Enter to continue the battle..."
                _ <- getLine
                gameLoop newGame

-- | Display board showing attack results
displayBoardForAttacks :: Board -> Set.Set Position -> String
displayBoardForAttacks board attacks =
    let header = "   " ++ concatMap (\i -> show i ++ " ") [0..boardSize-1] ++ "\n"
        rows = map (displayAttackRow board attacks) [0..boardSize-1]
    in header ++ unlines rows

displayAttackRow :: Board -> Set.Set Position -> Int -> String
displayAttackRow board attacks row =
    let rowHeader = show row ++ "  "
        cells = map (displayAttackCell board attacks) [Position row col | col <- [0..boardSize-1]]
    in rowHeader ++ unwords cells

displayAttackCell :: Board -> Set.Set Position -> Position -> String
displayAttackCell board attacks pos
    | pos `Set.member` attacks =
        case Map.lookup pos (cells board) of
            Just (Hit _) -> "X"  -- Hit
            Just Miss -> "O"     -- Miss
            _ -> "?"             -- Shouldn't happen
    | otherwise = "."            -- Not attacked yet

-- | Get a move from human player
getHumanMove :: IO Position
getHumanMove = do
    putStrLn "🎯 Choose your target coordinates:"
    putStrLn "   Format: Letter+Number (A3, B7) or Row,Column (0,3, 2,7)"
    putStr "⚓ Enter attack coordinates: "
    hFlush stdout
    input <- getLine
    case parsePosition input of
        Just pos -> return pos
        Nothing -> do
            putStrLn "❌ Invalid coordinates! Please try again."
            putStrLn "   Examples: A3, B7, or 0,3, 2,7"
            getHumanMove
