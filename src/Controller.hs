{-# language NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | gameStatus gstate  == Play
  = -- We show a new random number
    let ghosts' = map (ghostGo (maze gstate) (player gstate)) (ghosts gstate)
        player' = playerMove (playerTurn (maze gstate) (player gstate)) (maze gstate)
    in  return gstate { ghosts = ghosts', player = player' }

  | otherwise
  = -- Just update the elapsed time
    return $ gstate 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
    | c == 'w' = gstate { player = changePlayerDirection (player gstate) North }
    | c == 'a' = gstate { player = changePlayerDirection (player gstate) West }
    | c == 's' = gstate { player = changePlayerDirection (player gstate) South }
    | c == 'd' = gstate { player = changePlayerDirection (player gstate) East }
inputKey _ gstate = gstate -- Otherwise keep the same

-- General functionality
move :: Coordinate -> Direction -> Coordinate
move (x, y) North = (x, y + 1)
move (x, y) East  = (x + 1, y)
move (x, y) South = (x, y - 1)
move (x, y) West  = (x - 1, y)

changePlayerDirection :: Player -> Direction -> Player
changePlayerDirection player direction = player { nextDirection = direction }

-- The player and ghosts should not be able to turn 180 degrees
turn :: Direction -> Direction -> Direction
turn currentD inputD | currentD == inputD = currentD
                     | otherwise          = inputD

reverseDirection :: Direction -> Direction
reverseDirection North = South
reverseDirection East  = West
reverseDirection South = North
reverseDirection West  = East

checkForIntersection :: Maze -> Coordinate -> Direction -> [Direction]
checkForIntersection maze position currentDirection = [x | x <- [North, East, South, West], helper position x maze && x /= reverseDirection currentDirection]
  where
    helper :: Coordinate -> Direction -> Maze -> Bool
    helper coord direction maze = isTileValid (whatTile maze (move coord direction))

    isTileValid :: TileOption -> Bool
    isTileValid W = False
    isTileValid _ = True

-- Player functionality
playerMove :: Player -> Maze -> Player
playerMove player maze | whatTile maze (move (playerPosition player) (playerDirection player)) /= W = 
    let playerPosition' = move (playerPosition player) (playerDirection player)
    in player { playerPosition = playerPosition' }
                       | otherwise = player

playerTurn :: Maze -> Player -> Player
playerTurn maze player | (checkForIntersection maze (playerPosition player) (playerDirection player)) `has` (nextDirection player) = 
    let playerDirection' = turn (playerDirection player) (nextDirection player)
    in player { playerDirection = playerDirection' }
                       | otherwise = player
  where
    has :: Eq a => [a] -> a -> Bool
    has [] _                 = False
    has (x:xs) y | x == y    = True
                 | otherwise = xs `has` y


-- Ghost functionality
ghostMove :: Ghost -> Maze -> Ghost
ghostMove ghost maze | whatTile maze (move (ghostPosition ghost) (ghostDirection ghost)) /= W = 
    let ghostPosition' = move (ghostPosition ghost) (ghostDirection ghost)
    in ghost { ghostPosition = ghostPosition' }
                     | otherwise = ghost

data GamePathingTree = Node Direction [GamePathingTree] | Leaf | PacMan
  deriving (Eq, Show)

flatten :: GamePathingTree -> [Direction]
flatten Leaf                = []
flatten PacMan              = []
flatten (Node direction xs) = direction : concatMap flatten xs

flattenWeird :: GamePathingTree -> [GamePathingTree]
flattenWeird Leaf        = [Leaf]
flattenWeird PacMan      = [PacMan]
flattenWeird (Node _ xs) = Leaf : concatMap flattenWeird xs

countTilesAway :: [GamePathingTree] -> Int
countTilesAway []            = 0
countTilesAway (Leaf : xs)   = 1 + countTilesAway xs
countTilesAway (PacMan : xs) = 1
countTilesAway (_ : xs)      = 1 + countTilesAway xs

-- Both Leaf and PacMan should never be used here. This method should only be called when we need to know where we want to go next. Using maybe code here would only make the other code more complex when it really should just not happen.
nextValue :: Int -> [Direction] -> Direction
nextValue _ []       = West
nextValue 0 (x : xs) = x
nextValue i (x : xs) = nextValue (i - 1) xs

ghostGo :: Maze -> Player -> Ghost -> Ghost
ghostGo maze player ghost =
    let possibleDirections = checkForIntersection maze (ghostPosition ghost)
    in  ghostMove (getNextDirection maze (playerPosition player) ghost) maze
  where
    getNextDirection :: Maze -> Coordinate -> Ghost -> Ghost
    getNextDirection maze playerCoord ghost@Ghost { ghostAlgorithm = ShortPath } = 
        let tree = shortestPath maze ghost { tilesAway = (tilesAway ghost) + 2 } playerCoord
        in  ghost { ghostDirection = nextValue 1 (flatten tree), tilesAway = countTilesAway (flattenWeird tree) }
    getNextDirection maze playerCoord ghost = undefined


shortestPath :: Maze -> Ghost -> Coordinate -> GamePathingTree
shortestPath maze ghost playerCoord = sPAlgorithm maze playerCoord ghost
  where
    sPAlgorithm :: Maze -> Coordinate -> Ghost -> GamePathingTree
    sPAlgorithm maze playerCoord ghost@Ghost { tilesAway = 0 } | (ghostPosition ghost) == playerCoord = PacMan
                                                               | otherwise                  = Leaf
    sPAlgorithm maze playerCoord ghost | (ghostPosition ghost) == playerCoord = PacMan
                                                 | otherwise                  =
                                                 let intersections = checkForIntersection maze (ghostPosition ghost) (ghostDirection ghost)
                                                     children = map (sPAlgorithm maze playerCoord) [ghostMove (ghost { ghostDirection = x, tilesAway = (tilesAway ghost) - 1 }) maze | x <- intersections]
                                                 in if all (== Leaf) children then Leaf else Node (ghostDirection ghost) children

--Collision
checkForCollision :: Ghost -> Player -> (Ghost, Player)
checkForCollision ghost@Ghost { ghostState, ghostAlgorithm, ghostPosition, ghostDirection, tilesAway } player@Player { playerState, playerPowerUp, playerPosition, playerDirection, nextDirection } 
    | ghostPosition == playerPosition = handleCollision ghost player
    | otherwise                       = (ghost, player)

handleCollision :: Ghost -> Player -> (Ghost, Player)
handleCollision Ghost { ghostState, ghostAlgorithm, ghostPosition, ghostDirection, tilesAway } player@Player { playerState, playerPowerUp = Active i, playerPosition, playerDirection, nextDirection } =
    (Ghost { ghostState = Eaten, ghostAlgorithm, ghostPosition, ghostDirection, tilesAway }, player)
handleCollision ghost@Ghost { ghostState = Chasing, ghostAlgorithm, ghostPosition, ghostDirection, tilesAway } player@Player { playerState, playerPowerUp, playerPosition, playerDirection, nextDirection } =
    (ghost, Player { playerState = Dead, playerPowerUp, playerPosition, playerDirection, nextDirection })
handleCollision ghost player = (ghost, player)