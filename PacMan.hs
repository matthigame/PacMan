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
type Coordinate = (Int, Int)
type Tile = TileOption
type Maze = [[Tile]]

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

emptyMaze :: Maze
emptyMaze = [[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []]

{-
UpdateMaze :: GameState -> GameState
UpdateMaze GameState { maze, points, gameStatus, ghosts, p@Player { _, playerPowerUp, (x, y), _, _ } } 
	| ((maze !! y) !! x) == P = let points' = points + 10
								    maze'   = GenerateNewMaze maze (x, y) EmptyMaze

								in GameState { maze', points', gameStatus, ghosts, p} 
	| ((maze !! y) !! x) == S = GameState { maze, points, gameStatus, ghosts, p} 
	| otherwise               = GameState { maze, points, gameStatus, ghosts, p} 

GenerateNewMaze :: Maze -> Coordinate -> Maze -> Maze
GenerateNewMaze maze (x, y) accMaze = 
-}

-- General functionality
move :: Coordinate -> Direction -> Coordinate
move (x, y) North = (x, y - 1)
move (x, y) East  = (x + 1, y)
move (x, y) South = (x, y + 1)
move (x, y) West  = (x - 1, y)

-- The player and ghosts should not be able to turn 180 degrees
turn :: Direction -> Direction -> Direction
turn currentD inputD | currentD == inputD = currentD
                     | otherwise          = inputD

reverseDirection :: Direction -> Direction
reverseDirection North = South
reverseDirection East  = West
reverseDirection South = North
reverseDirection West  = East

checkForIntersection :: Maze -> Coordinate -> [Direction]
checkForIntersection maze position = [x | x <- [North, East, South, West], helper position x maze]
  where
    helper :: Coordinate -> Direction -> Maze -> Bool
    helper coord direction maze = isTileValid (whatTile maze (move coord direction))

    isTileValid :: TileOption -> Bool
    isTileValid W = False
    isTileValid _ = True


-- Maze functionality
-- Returns what the tile is
whatTile :: Maze -> Coordinate -> TileOption
whatTile maze (x, y) = (maze !! y) !! x



-- Player functionality
playerMove :: Player -> Player
playerMove Player { playerState, playerPowerUp, playerPosition, playerDirection, nextDirection } = 
    let playerPosition' = move playerPosition playerDirection
    in Player { playerState, playerPowerUp, playerPosition = playerPosition', playerDirection, nextDirection }

playerTurn :: Maze -> Player -> Player
playerTurn maze Player { playerState, playerPowerUp, playerPosition, playerDirection, nextDirection } | (checkForIntersection maze playerPosition) `has` nextDirection = 
    let playerDirection' = turn playerDirection nextDirection
    in Player { playerState, playerPowerUp, playerPosition, playerDirection = playerDirection', nextDirection }
  where
    has :: Eq a => [a] -> a -> Bool
    has [] _                 = False
    has (x:xs) y | x == y    = True
                 | otherwise = xs `has` y


-- Ghost functionality
ghostMove :: Ghost -> Ghost
ghostMove Ghost { ghostState, ghostAlgorithm, ghostPosition, ghostDirection, tilesAway } = 
    let ghostPosition' = move ghostPosition ghostDirection
    in Ghost { ghostState, ghostAlgorithm, ghostPosition = ghostPosition', ghostDirection, tilesAway}

data GamePathingTree = Node Direction [GamePathingTree] | Leaf | PacMan
  deriving (Eq, Show)

-- First coordinate is the coordinate of the ghost, the second coordinate is the coordinate of the player, z is how many tiles away the ghost was the last time. This helps us choose how many times we should do recursion.
shortestPath :: Maze -> Coordinate -> Coordinate -> Int -> Direction -> GamePathingTree
shortestPath maze ghostCoord playerCoord z ghostDirection = sPAlgorithm maze ghostCoord playerCoord z ghostDirection

sPAlgorithm :: Maze -> Coordinate -> Coordinate -> Int -> Direction -> GamePathingTree
sPAlgorithm maze g p 0 _ | g == p    = PacMan
                         | otherwise = Leaf
sPAlgorithm maze g@(x, y) p@(a, b) z direction | g == p    = PacMan
                                               | otherwise = 
                                               let g' = move g direction
                                                   children = map (sPAlgorithm maze g' p (z - 1)) [e | e <- checkForIntersection maze g', e /= reverseDirection direction]
                                               in if all (== Leaf) children then Leaf else Node direction children
