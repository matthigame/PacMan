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
  | checkForWin (maze gstate) (0, 0)      = finishGame (Win 60) (points gstate)
  | (playerState (player gstate)) == Dead = finishGame (Lose 60) (points gstate)
  | otherwise                             =
    let randomGen       = maybe (mkStdGen 1) id (randomGenerator gstate)
        gstate'         = updateMaze gstate
        gstate''        = updatePlayer gstate' 
        (ghosts', gen') = updateGhosts randomGen (ghosts gstate'') gstate''
        gstate'''       = gstate'' { ghosts = ghosts', totalSteps = (totalSteps gstate'') + 1, randomGenerator = Just gen' }
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
                      in  gstate { player = player''', ghosts = ghosts' }

updateGhosts :: StdGen -> Ghosts -> GameState -> (Ghosts, StdGen)
updateGhosts gen [] _ = ([], gen)
updateGhosts gen (ghost : ghosts) gstate | ((totalSteps gstate) `mod` 3) == 1 = let (ghost', gen')   = ghostGo gen (maze gstate) (player gstate) ghost
                                                                                    (ghosts', gen'') = updateGhosts gen' ghosts gstate
                                                                                in (ghost' : ghosts', gen'')
                                         | otherwise                          = (ghost : ghosts, gen)

updatePlayerPowerUp :: Player -> Player
updatePlayerPowerUp player@Player { playerPowerUp = Active 0 } = player { playerPowerUp = Inactive }
updatePlayerPowerUp player@Player { playerPowerUp = Active i } = player { playerPowerUp = Active (i - 1)}
updatePlayerPowerUp player                                     = player

finishGame :: GameStatus -> Int -> IO GameState
finishGame (Win i) score  = do 
    _ <- writeScore score
    return animationState { gameStatus = Win i }
finishGame (Lose i) score = do
    _ <- writeScore score
    return animationState { gameStatus = Lose i }
finishGame _ _ = return initialState

writeScore :: Int -> IO ()
writeScore score = do
    highScore <- currentHighScore
    if ((read highScore) < score) then writeFile "HighScores.txt" (show score) else return ()

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

--General functionality
pauseState :: GameStatus -> GameStatus
pauseState  Play = Pause
pauseState Pause = Play
pauseState state = state

move :: Coordinate -> Direction -> Coordinate
move (x, y) North = (x, y + 1)
move (x, y) East  = (x + 1, y)
move (x, y) South = (x, y - 1)
move (x, y) West  = (x - 1, y)

--changes the nextDirection from the player, this means the next time they hit an intersection that allows them to go this direction, they will automatically turn.
changePlayerDirection :: Player -> Direction -> Player
changePlayerDirection player direction = player { nextDirection = direction }

reverseDirection :: Direction -> Direction
reverseDirection North = South
reverseDirection East  = West
reverseDirection South = North
reverseDirection West  = East

-- this method checks if there are multiple direction the player or ghosts could go and returns these. 
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
    let playerDirection' = nextDirection player
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
  deriving Eq

getInt :: GamePathingTree -> Int
getInt Leaf         = 1000
getInt (PacMan _)   = 0
getInt (Node _ i _) = i

flatten :: GamePathingTree -> [Direction]
flatten Leaf                  = []
flatten (PacMan direction)    = [direction]
flatten (Node direction _ xs) = direction : concatMap flatten xs

-- The input here should never be an empty list. Using maybe code here would only make the other code more complex when it really should just not happen.
nextValue :: Int -> [Direction] -> Direction
nextValue _ []      = West
nextValue 0 (x : _) = x
nextValue i (_ : xs) = nextValue (i - 1) xs

ghostGo :: StdGen -> Maze -> Player -> Ghost -> (Ghost, StdGen)
ghostGo gen maze player@Player { playerPowerUp = Inactive } ghost@Ghost { ghostState = Chasing} = let (nextDirection, gen') = getNextDirection ghost 
                                                                                                  in (ghostMove nextDirection maze, gen')
  where
    getNextDirection :: Ghost -> (Ghost, StdGen)
    getNextDirection ghost'@Ghost { ghostAlgorithm = ShortPath } = 
        let tree = shortestPath maze (playerPosition player) ghost' { tilesAway = (tilesAway ghost) + 3 }
        in  (ghost { ghostDirection = nextValue 1 (flatten tree), tilesAway = if tree == Leaf then (tilesAway ghost) else getInt tree }, gen)

    getNextDirection ghost'@Ghost { ghostAlgorithm = FuturePath } = 
        let (playerPos, gen') = futurePosition gen player 5
            tree = shortestPath maze playerPos ghost' { tilesAway = (tilesAway ghost) + 8 }
        in  (ghost { ghostDirection = nextValue 1 (flatten tree), tilesAway = if tree == Leaf then (tilesAway ghost) else getInt tree }, gen')

    getNextDirection ghost'@Ghost { ghostAlgorithm = PastPath } = 
        let (playerPos, gen') = pastPosition gen player 5
            tree              = shortestPath maze playerPos ghost' { tilesAway = (tilesAway ghost) + 8 }
        in  (ghost { ghostDirection = nextValue 1 (flatten tree), tilesAway = if tree == Leaf then (tilesAway ghost) else getInt tree }, gen')

    getNextDirection Ghost { ghostAlgorithm = Random } = let (ranDirection, gen') = randomDirection gen maze ghost
                                                         in (ghost { ghostDirection = ranDirection }, gen')

    --looks where a future position of the player could be
    futurePosition :: StdGen -> Player -> Int -> (Coordinate, StdGen)
    futurePosition gen' player' 0 = ((playerPosition player'), gen')
    futurePosition _ player' i = let (nextPosition, gen') = nextPlayerPosition player' 
                                 in futurePosition gen' nextPosition (i - 1) 

    nextPlayerPosition :: Player -> (Player, StdGen)
    nextPlayerPosition player' = let possibleDirections     = checkForIntersection maze (playerPosition player') (playerDirection player')
                                     (randomNumber, newGen) = randomR (0 :: Int, ((length possibleDirections) - 1) :: Int) gen
                                 in  (playerMove (player' { playerDirection = (possibleDirections !! randomNumber) }) maze, newGen)

    --looks where a past position of the player could be
    pastPosition :: StdGen -> Player -> Int -> (Coordinate, StdGen)
    pastPosition gen' player' 0 = ((playerPosition player'), gen')
    pastPosition _ player' i = let (nextPosition, gen') = nextPlayerPosition (player' { playerDirection = reverseDirection (playerDirection player') }) 
                               in pastPosition gen' nextPosition (i - 1)

--when the powerup is active all of the ghosts use random movement
ghostGo gen maze Player { playerPowerUp = Active _ } ghost@Ghost { ghostState = Chasing} = let (randDirection, gen') = randomDirection gen maze ghost 
                                                                                           in (ghostMove (ghost { ghostDirection = randDirection, tilesAway = 40 }) maze, gen')
ghostGo gen _ _ ghost@Ghost {ghostState = Eaten 0}                                       = (ghost { ghostState = Chasing, tilesAway = 40 }, gen)
ghostGo gen _ _ ghost@Ghost {ghostState = Eaten i}                                       = (ghost { ghostState = Eaten (i - 1) }, gen)

randomDirection :: StdGen -> Maze -> Ghost -> (Direction, StdGen)
randomDirection gen maze ghost' = let possibleDirections = checkForIntersection maze (ghostPosition ghost') (ghostDirection ghost')
                                      (randomNumber, newGen) = randomR (0 :: Int, ((length possibleDirections) - 1) :: Int) gen
                                  in  (possibleDirections !! randomNumber, newGen)

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
