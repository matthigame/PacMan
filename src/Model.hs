-- | This module contains the data types
--   which represent the state of the game
module Model where

-- Our GameState
data GameState = GameState {
    maze :: Maze
,   points :: Int
,   gameStatus :: GameStatus
,   ghosts :: Ghosts
,   player :: Player
}

--
data GameStatus = Win | Lose | Play | Pause
  deriving Eq
--type Points :: Int
data Direction = North | East | South | West
  deriving Eq

instance Show Direction where
  show North = "North"
  show East  = "East"
  show South = "South"
  show West  = "West"

-- Everything that is used for the player
data PlayerState = Dead | Alive
data PlayerPowerUp = Inactive | Active Int
--data PlayerPosition = Coordinate
data Player = Player {
    playerState :: PlayerState
,   playerPowerUp :: PlayerPowerUp
,   playerPosition :: Coordinate
,   playerDirection :: Direction
,   nextDirection :: Direction
}

-- Everything that is used for the ghosts
data GhostState = Eaten | Chasing | Respawning
data GhostAlgorithm = ShortPath | FuturePath | PastPath | ShyPath | Guard Coordinate | Random
--type GhostPosition = Coordinate
data Ghost = Ghost {
    ghostState :: GhostState
,   ghostAlgorithm :: GhostAlgorithm
,   ghostPosition :: Coordinate
,   ghostDirection  :: Direction
,   tilesAway :: Int
}
type Ghosts = [Ghost]

-- Everything that is used for the maze
--         (Wall, Empty, Pellet, SuperPellet)
data TileOption = W | E | P | S
  deriving Eq

type Coordinate = (Int, Int)
type Tile = TileOption
type Maze = [[Tile]]

-- Maze functionality
-- Returns what the tile is
whatTile :: Maze -> Coordinate -> TileOption
whatTile maze@(a : as) (x, y) | x < length a && x > 0 && y < length maze && y > 0 = (maze !! y) !! x
                              | otherwise = W

initialState :: GameState
initialState = GameState { maze = testBoard
                           , points = 0
                           , gameStatus = Play
                           , ghosts = [Ghost { ghostState = Chasing, ghostAlgorithm = ShortPath, ghostPosition = (1, 1), ghostDirection = East, tilesAway = 40 }]
                           , player = Player { playerState = Alive, playerPowerUp = Inactive, playerPosition = (13, 23), playerDirection = West, nextDirection = East } }

-- The basic starting maze
testBoard :: Maze
testBoard = [[W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W],
             [W, P, P, P, P, P, P, P, P, P, P, P, P, W, W, P, P, P, P, P, P, P, P, P, P, P, P, W],
             [W, P, W, W, W, W, P, W, W, W, W, W, P, W, W, P, W, W, W, W, W, P, W, W, W, W, P, W],
             [W, S, W, W, W, W, P, W, W, W, W, W, P, W, W, P, W, W, W, W, W, P, W, W, W, W, S, W],
             [W, P, W, W, W, W, P, W, W, W, W, W, P, W, W, P, W, W, W, W, W, P, W, W, W, W, P, W],
             [W, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, W],
             [W, P, W, W, W, W, P, W, W, P, W, W, W, W, W, W, W, W, P, W, W, P, W, W, W, W, P, W],
             [W, P, W, W, W, W, P, W, W, P, W, W, W, W, W, W, W, W, P, W, W, P, W, W, W, W, P, W],
             [W, P, P, P, P, P, P, W, W, P, P, P, P, W, W, P, P, P, P, W, W, P, P, P, P, P, P, W],
             [W, W, W, W, W, W, P, W, W, W, W, W, P, W, W, P, W, W, W, W, W, P, W, W, W, W, W, W],
             [W, W, W, W, W, W, P, W, W, W, W, W, P, W, W, P, W, W, W, W, W, P, W, W, W, W, W, W],
             [W, W, W, W, W, W, P, W, W, P, P, P, P, P, P, P, P, P, P, W, W, P, W, W, W, W, W, W],
             [W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W],
             [W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W],
             [W, P, P, P, P, P, P, P, P, P, W, W, W, W, W, W, W, W, P, P, P, P, P, P, P, P, P, W],
             [W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W],
             [W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W],
             [W, W, W, W, W, W, P, W, W, P, P, P, P, P, P, P, P, P, P, W, W, P, W, W, W, W, W, W],
             [W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W],
             [W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W],
             [W, P, P, P, P, P, P, P, P, P, P, P, P, W, W, P, P, P, P, P, P, P, P, P, P, P, P, W],
             [W, P, W, W, W, W, P, W, W, W, W, W, P, W, W, P, W, W, W, W, W, P, W, W, W, W, P, W],
             [W, P, W, W, W, W, P, W, W, W, W, W, P, W, W, P, W, W, W, W, W, P, W, W, W, W, P, W],
             [W, S, P, P, W, W, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, W, W, P, P, S, W],
             [W, W, W, P, W, W, P, W, W, P, W, W, W, W, W, W, W, W, P, W, W, P, W, W, P, W, W, W],
             [W, W, W, P, W, W, P, W, W, P, W, W, W, W, W, W, W, W, P, W, W, P, W, W, P, W, W, W],
             [W, P, P, P, P, P, P, W, W, P, P, P, P, W, W, P, P, P, P, W, W, P, P, P, P, P, P, W],
             [W, P, W, W, W, W, W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W, W, W, W, W, P, W],
             [W, P, W, W, W, W, W, W, W, W, W, W, P, W, W, P, W, W, W, W, W, W, W, W, W, W, P, W],
             [W, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, W],
             [W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W]]