module Ship
    ( Ship(..)
    , ShipType(..)
    , Orientation(..)
    , Position(..)
    , createShip
    , shipLength
    , shipPositions
    ) where

-- | Position on the board (row, column)
data Position = Position Int Int
    deriving (Show, Eq, Ord)

-- | Ship orientation
data Orientation = Horizontal | Vertical
    deriving (Show, Eq)

-- | Types of ships with their lengths
data ShipType = Carrier | Battleship | Cruiser | Submarine | Destroyer
    deriving (Show, Eq)

-- | Ship data structure
data Ship = Ship
    { shipType :: ShipType
    , startPos :: Position
    , orientation :: Orientation
    , hits :: [Position]  -- Positions that have been hit
    } deriving (Show, Eq)

-- | Get the length of a ship type
shipLength :: ShipType -> Int
shipLength Carrier = 5
shipLength Battleship = 4
shipLength Cruiser = 3
shipLength Submarine = 3
shipLength Destroyer = 2

-- | Create a new ship
createShip :: ShipType -> Position -> Orientation -> Ship
createShip sType pos orient = Ship sType pos orient []

-- | Get all positions occupied by a ship
shipPositions :: Ship -> [Position]
shipPositions (Ship sType (Position row col) orient _) =
    let len = shipLength sType
    in case orient of
        Horizontal -> [Position row (col + i) | i <- [0..len-1]]
        Vertical   -> [Position (row + i) col | i <- [0..len-1]]
