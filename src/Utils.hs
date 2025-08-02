module Utils
    ( displayBoard
    , displayBoardWithShips
    , parsePosition
    , clearScreen
    , displayGameTitle
    , displayGameStats
    ) where

import Ship
import Board
import qualified Data.Map as Map
import Data.Char (toUpper, isDigit)

-- ANSI color codes for terminals (disabled for Windows compatibility)
reset :: String
reset = ""

red :: String
red = ""

green :: String
green = ""

blue :: String
blue = ""

cyan :: String
cyan = ""

yellow :: String
yellow = ""

magenta :: String
magenta = ""

bold :: String
bold = ""

-- | Clear the screen (works on most terminals)
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Display game title with style
displayGameTitle :: IO ()
displayGameTitle = do
    putStrLn $ cyan ++ bold ++ "+---------------------------------------------+" ++ reset
    putStrLn $ cyan ++ bold ++ "|    BATTLESHIP - HASKELL EDITION           |" ++ reset  
    putStrLn $ cyan ++ bold ++ "|                                            |" ++ reset
    putStrLn $ cyan ++ bold ++ "|     Functional Programming Meets Naval    |" ++ reset
    putStrLn $ cyan ++ bold ++ "|              Combat Strategy!             |" ++ reset
    putStrLn $ cyan ++ bold ++ "+---------------------------------------------+" ++ reset
    putStrLn ""

-- | Display game statistics
displayGameStats :: Int -> Int -> IO ()
displayGameStats playerShips computerShips = do
    putStrLn $ yellow ++ "BATTLE STATUS:" ++ reset
    putStrLn $ "   Your ships remaining: " ++ green ++ show playerShips ++ reset
    putStrLn $ "   Enemy ships remaining: " ++ red ++ show computerShips ++ reset
    putStrLn ""

-- | Display the board without revealing ships (for opponent's view)
displayBoard :: Board -> String
displayBoard board =
    let header = createHeader
        rows = map (displayRow board False) [0..boardSize-1]
        footer = createFooter
    in header ++ unlines rows ++ footer

-- | Display the board with ships visible (for player's own board)
displayBoardWithShips :: Board -> String
displayBoardWithShips board =
    let header = createHeader
        rows = map (displayRow board True) [0..boardSize-1]
        footer = createFooter
    in header ++ unlines rows ++ footer

-- | Create fancy header for the board
createHeader :: String
createHeader = 
    cyan ++ "   +---------------------+\n" ++ reset ++
    cyan ++ "   | " ++ reset ++ yellow ++ "A B C D E F G H I" ++ reset ++ cyan ++ " |\n" ++ reset ++
    cyan ++ "   +---------------------+\n" ++ reset

-- | Create footer for the board
createFooter :: String
createFooter = cyan ++ "   +---------------------+" ++ reset

-- | Display a single row of the board
displayRow :: Board -> Bool -> Int -> String
displayRow board showShips row =
    let rowHeader = cyan ++ show row ++ reset ++ cyan ++ "  | " ++ reset
        cellList = map (displayCell board showShips) [Position row col | col <- [0..boardSize-1]]
        rowFooter = cyan ++ " |" ++ reset
    in rowHeader ++ unwords cellList ++ rowFooter

-- | Display a single cell with colors
displayCell :: Board -> Bool -> Position -> String
displayCell board showShips pos =
    case Map.lookup pos (cells board) of
        Nothing -> "?"
        Just Empty -> blue ++ "~" ++ reset
        Just Miss -> cyan ++ "O" ++ reset
        Just (Hit _) -> red ++ bold ++ "X" ++ reset
        Just (HasShip _) -> if showShips then green ++ "#" ++ reset else blue ++ "~" ++ reset

-- | Parse a position from user input (e.g., "A3" or "3,4")
parsePosition :: String -> Maybe Position
parsePosition input
    | length input == 2 = parseLetterNumber input
    | ',' `elem` input = parseCommaSeparated input
    | otherwise = Nothing
  where
    parseLetterNumber [letter, digit]
        | letter `elem` ['A'..'I'] && isDigit digit =
            let row = fromEnum (toUpper letter) - fromEnum 'A'
                col = read [digit]
            in if col >= 0 && col < boardSize
               then Just (Position row col)
               else Nothing
    parseLetterNumber _ = Nothing
    
    parseCommaSeparated str =
        case break (== ',') str of
            (rowStr, ',':colStr) ->
                case (reads rowStr, reads colStr) of
                    ([(row, "")], [(col, "")]) ->
                        if row >= 0 && row < boardSize && col >= 0 && col < boardSize
                        then Just (Position row col)
                        else Nothing
                    _ -> Nothing
            _ -> Nothing
