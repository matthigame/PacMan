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
,   totalSteps :: Int
}

--
data GameStatus = Win Int | Lose Int | Play | Pause | Home
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
  deriving Eq
data PlayerPowerUp = Inactive | Active Int
  deriving Eq
--data PlayerPosition = Coordinate
data Player = Player {
    playerState :: PlayerState
,   playerPowerUp :: PlayerPowerUp
,   playerPosition :: Coordinate
,   playerDirection :: Direction
,   nextDirection :: Direction
}

-- Everything that is used for the ghosts
data GhostState = Eaten Int | Chasing
  deriving Eq
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
whatTile maze (x, y) | x < mazeWidth && x > 0 && y < mazeLength && y > 0 = (maze !! y) !! x
                     | otherwise                                         = W

initialState :: GameState
initialState = GameState { maze = testBoard
                           , points = 0
                           , gameStatus = Home
                           , ghosts = [Ghost { ghostState = Chasing, ghostAlgorithm = ShortPath, ghostPosition = (1, 1), ghostDirection = East, tilesAway = 40 },
                                       Ghost { ghostState = Chasing, ghostAlgorithm = FuturePath, ghostPosition = (1, 29), ghostDirection = East, tilesAway = 40 },
                                       Ghost { ghostState = Chasing, ghostAlgorithm = PastPath, ghostPosition = (25, 1), ghostDirection = East, tilesAway = 40 },
                                       Ghost { ghostState = Chasing, ghostAlgorithm = Random, ghostPosition = (25, 29), ghostDirection = East, tilesAway = 40 }]
                           , player = Player { playerState = Alive, playerPowerUp = Inactive, playerPosition = (13, 23), playerDirection = West, nextDirection = West } 
                           , totalSteps = 0
                           }

animationState = GameState { maze = testBoard
                           , points = 0
                           , gameStatus = Home
                           , ghosts = [Ghost { ghostState = Chasing, ghostAlgorithm = ShortPath, ghostPosition = (2, 30), ghostDirection = East, tilesAway = 40 },
                                       Ghost { ghostState = Chasing, ghostAlgorithm = FuturePath, ghostPosition = (10, 30), ghostDirection = East, tilesAway = 40 },
                                       Ghost { ghostState = Chasing, ghostAlgorithm = PastPath, ghostPosition = (18, 30), ghostDirection = East, tilesAway = 40 },
                                       Ghost { ghostState = Chasing, ghostAlgorithm = Random, ghostPosition = (26, 30), ghostDirection = East, tilesAway = 40 }]
                           , player = Player { playerState = Alive, playerPowerUp = Inactive, playerPosition = (1, 10), playerDirection = East, nextDirection = East } 
                           , totalSteps = 0
                           }

-- The basic starting maze
testBoard :: Maze
testBoard = [[W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W, W],
             [W, E, P, P, P, P, P, P, P, P, P, P, P, W, W, P, P, P, P, P, P, P, P, P, P, P, P, W],
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
             [W, E, E, E, E, E, P, P, P, P, W, W, W, W, W, W, W, W, P, P, P, P, E, E, E, E, E, W],
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

emptyMaze :: Maze
emptyMaze = [[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []]

mazeLength :: Int
mazeLength = length testBoard

mazeWidth :: Int
mazeWidth  = length (testBoard !! 0)
