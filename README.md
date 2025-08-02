# Battleship Game in Haskell

A command-line implementation of the classic Battleship game written in Haskell.

## Features

- Human vs Computer gameplay
- 9x9 game board
- 5 ships of different sizes (Carrier: 5, Battleship: 4, Cruiser: 3, Submarine: 3, Destroyer: 2)
- Random ship placement
- Clear visual board representation
- Input validation

## Prerequisites

- GHC (Glasgow Haskell Compiler) 8.10 or later
- Cabal build tool

## Building and Running

1. **Build the project:**
   ```bash
   cabal build
   ```

2. **Run the game:**
   ```bash
   cabal run battleship
   ```

## How to Play

1. The game starts with both you and the computer having ships randomly placed on your respective boards
2. You'll see two boards:
   - Your board (showing your ships as 'S')
   - Attack board (showing your previous attacks on the computer's board)
3. Enter coordinates to attack:
   - Letter-number format: `A3`, `B7`, etc. (A-I for rows, 0-8 for columns)
   - Comma-separated format: `0,3`, `2,7`, etc.
4. Results:
   - `X` = Hit
   - `O` = Miss
   - `.` = Not attacked yet
5. Sink all enemy ships to win!

## Project Structure

- `src/Ship.hs` - Ship data types and operations
- `src/Board.hs` - Game board logic and ship placement
- `src/Player.hs` - Player types and computer AI
- `src/Game.hs` - Game state management and turn logic
- `src/Utils.hs` - Utility functions for display and input parsing
- `src/Main.hs` - Main game loop and user interface

## Game Rules

- Ships cannot overlap
- Ships are placed randomly at the start
- You continue attacking until you miss
- First player to sink all opponent ships wins
- Board coordinates: rows A-I (0-8), columns 0-8

## Future Enhancements

- Player vs Player mode
- Configurable board size
- Smart computer AI (instead of random moves)
- Ship placement by user
- Save/load game state
- Network multiplayer
