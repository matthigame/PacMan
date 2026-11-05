{-# language NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@GameState { gameStatus = Play }
  = -- First we check for collision, then we move the ghosts and players.
    let gstate'   = updateMaze gstate
        gstate''  = updatePlayer gstate' 
        ghosts'   = updateGhosts (ghosts gstate'') gstate''
        gstate''' = if (checkForWin (maze gstate'') (0, 0)) then animationState { gameStatus = Win 60 } else gstate'' { ghosts = ghosts', totalSteps = (totalSteps gstate'') + 1 }
    in  return gstate'''
step secs gstate@GameState { gameStatus = Win i }  | i > 0     = return gstate { gameStatus = Win (i - 1), player = (player gstate) { playerPosition = playerAnimation (playerPosition (player gstate)) i }, ghosts = map (ghostAnimation i) (ghosts gstate) }
                                                   | otherwise = return initialState
step secs gstate@GameState { gameStatus = Lose i } | i > 0     = return gstate { gameStatus = Lose (i - 1), player = (player gstate) { playerPosition = playerAnimation (playerPosition (player gstate)) i}, ghosts = map (ghostAnimation i) (ghosts gstate)  }
                                                   | otherwise = return initialState
step secs gstate                                               = return gstate

updatePlayer :: GameState -> GameState
updatePlayer gstate = let (ghosts', player') = checkForCollision (ghosts gstate) (player gstate)
                          player'' = if ((playerState player') == Alive && ((totalSteps gstate) `mod` 2) == 1) then playerMove (playerTurn (maze gstate) player') (maze gstate) else player'
                          player''' = updatePlayerPowerUp player''
                      in  if (playerState player''') == Dead then animationState { gameStatus = Lose 60 } else gstate { player = player''', ghosts = ghosts' }

updateGhosts :: Ghosts -> GameState -> Ghosts
updateGhosts ghosts gstate = if ((totalSteps gstate) `mod` 3) == 1 then map (ghostGo (maze gstate) (player gstate)) ghosts else ghosts

updatePlayerPowerUp :: Player -> Player
updatePlayerPowerUp player@Player { playerPowerUp = Active 0 } = player { playerPowerUp = Inactive }
updatePlayerPowerUp player@Player { playerPowerUp = Active i } = player { playerPowerUp = Active (i - 1)}
updatePlayerPowerUp player                                     = player


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
    | c == 'w' = gstate { player = changePlayerDirection (player gstate) North }
    | c == 'a' = gstate { player = changePlayerDirection (player gstate) West }
    | c == 's' = gstate { player = changePlayerDirection (player gstate) South }
    | c == 'd' = gstate { player = changePlayerDirection (player gstate) East }
    | c == 'p' = gstate { gameStatus = pauseState (gameStatus gstate) }
inputKey (EventKey (SpecialKey KeySpace) _ _ _) GameState { gameStatus = Home } = initialState { gameStatus = Play }
inputKey _ gstate = gstate -- Otherwise keep the same

-- gameState changing
pauseState :: GameStatus -> GameStatus
pauseState  Play = Pause
pauseState Pause = Play
pauseState state = state

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
checkForIntersection maze position currentDirection = [x | x <- [North, East, South, West], helper x && x /= reverseDirection currentDirection]
  where
    helper :: Direction -> Bool
    helper direction = isTileValid (whatTile maze (move position direction))

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

data GamePathingTree = Node Direction Int [GamePathingTree] | Leaf | PacMan Direction
  deriving (Eq, Show)

getInt :: GamePathingTree -> Int
getInt Leaf         = 1000
getInt (PacMan _)   = 0
getInt (Node _ i _) = i

flatten :: GamePathingTree -> [Direction]
flatten Leaf                  = []
flatten (PacMan direction)    = [direction]
flatten (Node direction _ xs) = direction : concatMap flatten xs

-- Both Leaf and PacMan should never be used here. This method should only be called when we need to know where we want to go next. Using maybe code here would only make the other code more complex when it really should just not happen.
nextValue :: Int -> [Direction] -> Direction
nextValue _ []       = West
nextValue 0 (x : _) = x
nextValue i (_ : xs) = nextValue (i - 1) xs

ghostGo :: Maze -> Player -> Ghost -> Ghost
ghostGo maze player ghost@Ghost { ghostState = Chasing} = ghostMove (getNextDirection (playerPosition player) ghost) maze
  where
    getNextDirection :: Coordinate -> Ghost -> Ghost
    getNextDirection playerCoord ghost'@Ghost { ghostAlgorithm = ShortPath } = 
        let tree = shortestPath maze playerCoord ghost' { tilesAway = (tilesAway ghost) + 3 }
        in  ghost { ghostDirection = nextValue 1 (flatten tree), tilesAway = if tree == Leaf then (tilesAway ghost) else getInt tree }

    getNextDirection _ ghost'@Ghost { ghostAlgorithm = FuturePath } = 
        let tree = shortestPath maze (futurePosition player 5) ghost' { tilesAway = (tilesAway ghost) + 3 }
        in  ghost { ghostDirection = nextValue 1 (flatten tree), tilesAway = if tree == Leaf then (tilesAway ghost) else getInt tree }

    getNextDirection _ ghost'@Ghost { ghostAlgorithm = PastPath } = 
        let tree = shortestPath maze (pastPosition player 5) ghost' { tilesAway = (tilesAway ghost) + 3 }
        in  ghost { ghostDirection = nextValue 1 (flatten tree), tilesAway = if tree == Leaf then (tilesAway ghost) else getInt tree }

    getNextDirection playerCoord ghost'@Ghost { ghostAlgorithm = ShyPath } = -- Not yet implemented, is now just ShortPath
        let tree = shortestPath maze playerCoord ghost' { tilesAway = (tilesAway ghost) + 2 }
        in  ghost { ghostDirection = nextValue 1 (flatten tree), tilesAway = if tree == Leaf then (tilesAway ghost) else getInt tree }

    getNextDirection playerCoord ghost'@Ghost { ghostAlgorithm = Guard _ } = -- Not yet implemented, is now just ShortPath
        let tree = shortestPath maze playerCoord ghost' { tilesAway = (tilesAway ghost) + 2 }
        in  ghost { ghostDirection = nextValue 1 (flatten tree), tilesAway = if tree == Leaf then (tilesAway ghost) else getInt tree }

    getNextDirection _ Ghost { ghostAlgorithm = Random } = ghost { ghostDirection = randomDirection ghost }

    futurePosition :: Player -> Int -> Coordinate
    futurePosition player' 0 = (playerPosition player')
    futurePosition player' i = futurePosition (nextPlayerPosition player') (i - 1) 

    nextPlayerPosition :: Player -> Player
    nextPlayerPosition player' = let possibleDirections = checkForIntersection maze (playerPosition player') (playerDirection player')
                                     pureGen            = mkStdGen 137
                                     randomNumber       = fst (uniformR (0 :: Int, ((length possibleDirections) - 1) :: Int) pureGen)
                                 in  playerMove (player' { playerDirection = (possibleDirections !! randomNumber) }) maze

    pastPosition :: Player -> Int -> Coordinate
    pastPosition player' 0 = (playerPosition player')
    pastPosition player' i = pastPosition (nextPlayerPosition (player' { playerDirection = reverseDirection (playerDirection player') })) (i - 1)
    
    randomDirection :: Ghost -> Direction
    randomDirection ghost' = let possibleDirections = checkForIntersection maze (ghostPosition ghost') (ghostDirection ghost')
                                 pureGen            = mkStdGen 137
                                 randomNumber       = fst (uniformR (0 :: Int, ((length possibleDirections) - 1) :: Int) pureGen)
                             in  possibleDirections !! randomNumber

ghostGo _ _ ghost@Ghost {ghostState = Eaten 0} = ghost { ghostState = Chasing, tilesAway = 40 }
ghostGo _ _ ghost@Ghost {ghostState = Eaten i} = ghost { ghostState = Eaten (i - 1) }


shortestPath :: Maze -> Coordinate -> Ghost -> GamePathingTree
shortestPath _ playerCoord ghost@Ghost { tilesAway = 0 } | (ghostPosition ghost) == playerCoord = PacMan (ghostDirection ghost)
                                                         | otherwise                            = Leaf
shortestPath maze playerCoord ghost | (ghostPosition ghost) == playerCoord = PacMan (ghostDirection ghost)
                                    | otherwise                            =
                                      let intersections = checkForIntersection maze (ghostPosition ghost) (ghostDirection ghost)
                                          children = map (shortestPath maze playerCoord) [ghostMove (ghost { ghostDirection = x, tilesAway = (tilesAway ghost) - 1 }) maze | x <- intersections]
                                      in if all (== Leaf) children then Leaf else Node (ghostDirection ghost) (minimum (map getInt children) + 1) children

--Collision
checkForCollision :: Ghosts -> Player -> (Ghosts, Player)
checkForCollision allGhosts player = let bools = [(ghostPosition ghost) == (playerPosition player) | ghost <- allGhosts]
                                  in helper allGhosts bools player
  where
    helper :: Ghosts -> [Bool] -> Player -> (Ghosts, Player)
    helper (ghost : ghosts) (x : xs) p | x         = let ghost' = handleCollisionGhost ghost p
                                                         player' = handleCollisionPlayer ghost p
                                                         ghosts' = fst (helper ghosts xs player')
                                                     in (ghost' : ghosts', player')
                                       | otherwise = let both    = helper ghosts xs p
                                                         ghosts' = fst both
                                                         player' = snd both
                                                   in (ghost : ghosts', player')
    helper _ _ p = ([], p)

handleCollisionGhost :: Ghost -> Player -> Ghost
handleCollisionGhost ghost Player { playerPowerUp = Active _} = ghost { ghostState = Eaten 20 }
handleCollisionGhost ghost _                                  = ghost

handleCollisionPlayer :: Ghost -> Player -> Player
handleCollisionPlayer Ghost { ghostState = Chasing } player@Player { playerPowerUp = Inactive } = player { playerState = Dead }
handleCollisionPlayer _ player                                                                  = player

--Update the maze
updateMaze :: GameState -> GameState
updateMaze gstate | whatTile (maze gstate) (playerPosition (player gstate)) == P = gstate { maze = createNewMaze (maze gstate) (playerPosition (player gstate)) [] (0, 0), points = (points gstate) + 10 }
                  | whatTile (maze gstate) (playerPosition (player gstate)) == S = gstate { maze = createNewMaze (maze gstate) (playerPosition (player gstate)) [] (0, 0), player = (player gstate) { playerPowerUp = Active 60 } }
                  | otherwise                                                    = gstate

createNewMaze :: Maze -> Coordinate -> Maze -> Coordinate -> Maze
createNewMaze maze pMCoord newMaze (x, y) | y >= mazeLength = newMaze
                                          | otherwise       = createNewMaze maze pMCoord (newMaze ++ [(createRow [] 0)]) (x, y + 1)

  where
    createRow :: [Tile] -> Int -> [Tile]
    createRow tiles counter | counter >= mazeWidth    = tiles
                            | pMCoord == (counter, y) = createRow (tiles ++ [E]) (counter + 1)
                            | otherwise               = createRow (tiles ++ [(whatTile maze (counter, y))]) (counter + 1)

--Check for win
checkForWin :: Maze -> Coordinate -> Bool
checkForWin _ (-1, -1)                                     = True
checkForWin maze coord@(x, y) | (whatTile maze coord) == P = False
                              | otherwise                  = checkForWin maze nextCoord
  where
    nextCoord :: Coordinate
    nextCoord | y >= mazeLength = (-1, -1)
              | x >= mazeWidth  = (0, y + 1)
              | otherwise       = (x + 1, y)


--Winning and losing animation
playerAnimation :: Coordinate -> Int -> Coordinate
playerAnimation (x, y) i | i > 35    = (x + 1, y)
                         | i > 30    = (x, y + 1)
                         | i > 5    = (x - 1, y)
                         | otherwise = (x, y - 1)

ghostAnimation :: Int -> Ghost -> Ghost
ghostAnimation i ghost | i > 30    = let (x, y) = ghostPosition ghost
                                     in ghost { ghostPosition = (x, y - 1) }
                       | otherwise = let (x, y) = ghostPosition ghost
                                     in ghost { ghostPosition = (x, y + 1) }
