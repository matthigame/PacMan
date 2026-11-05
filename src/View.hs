{-# language NamedFieldPuns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure GameState { gameStatus = Home }          = drawHomeScreen
viewPure gstate@GameState { gameStatus = Win i }  = endGameAnimation gstate i "You win!"
viewPure gstate@GameState { gameStatus = Lose i } = endGameAnimation gstate i "You lose"
viewPure gState = let player'         = (player gState)
                      playerPosition' = (playerPosition player')
                  in  pictures [drawMaze (maze gState) playerPosition', drawPlayer player', cameraTranslate (drawGhosts (ghosts gState)) playerPosition', drawPoints (points gState)]

drawMaze :: Maze -> Coordinate -> Picture
drawMaze maze playerPosition = cameraTranslate (pictures (drawTiles (0, 0))) playerPosition
  where
    drawTiles :: Coordinate -> [Picture]
    drawTiles (-1, -1)        = []
    drawTiles coord@(x, y) = translate (15 * fromIntegral x - 200)  (15 * fromIntegral y - 200) (case whatTile maze coord of
      W -> color blue (rectangleSolid 15 15)
      E -> color black (rectangleSolid 15 15)
      P -> color yellow (circleSolid 2)
      S -> color yellow (circleSolid 5)) : drawTiles (nextCoordinate coord)

    nextCoordinate :: Coordinate -> Coordinate
    nextCoordinate (x, y) | x + 1 < mazeWidth  = (x + 1, y)
                          | y + 1 < mazeLength = (0, y + 1)
                          | otherwise          = (-1, -1)

drawPlayer :: Player -> Picture
drawPlayer player@Player { playerState = Alive} = let nextDirectionArrow = (direcToCoord (nextDirection player))
                                                      colour             = if (playerPowerUp player) == Inactive then yellow else green
                                                  in pictures [(color colour (circleSolid 5)), translate (8 * (fst nextDirectionArrow)) (8 * (snd nextDirectionArrow)) (color colour (rectangleWire 5 5))]
drawPlayer _                                    = Blank

direcToCoord :: Direction -> (Float, Float)
direcToCoord North = (0, 1)
direcToCoord East  = (1, 0)
direcToCoord South = (0, -1)
direcToCoord West  = (-1, 0)
                  

drawGhosts :: Ghosts -> Picture
drawGhosts ghosts = pictures (map drawGhost ghosts)
  where
    drawGhost :: Ghost -> Picture
    drawGhost ghost@Ghost { ghostState = Chasing } = translate (15 * fromIntegral (fst (ghostPosition ghost)) - 200) (15 * fromIntegral (snd (ghostPosition ghost)) - 200) (color red (circleSolid 5))
    drawGhost _                                    = Blank

drawPoints :: Int -> Picture
drawPoints i = translate (-300) (-300) (color white (text (show i)))

cameraTranslate :: Picture -> Coordinate -> Picture
cameraTranslate picture playerPosition = translate (-(15 * fromIntegral (fst playerPosition) - 200)) (-(15 * fromIntegral (snd playerPosition) - 200)) picture

drawHomeScreen :: Picture
drawHomeScreen = pictures [scale (0.9) (0.9) (translate (-230) (50) (color white (text "PacMan"))), scale (0.25) (0.25) (translate (-630) (-200) (color white (text "Press space to play")))]

endGameAnimation :: GameState -> Int -> String -> Picture
endGameAnimation gstate i string = pictures [textTransformations (text string) i, animationPartPacman (player gstate), drawGhosts (ghosts gstate)]

textTransformations :: Picture -> Int -> Picture
textTransformations picture i | odd (i `div` 3) = scale (0.6) (0.6) (translate (-300) (-50) (color white (picture)))
                              | otherwise = Blank

animationPartPacman :: Player -> Picture
animationPartPacman player = translate (15 * fromIntegral (fst (playerPosition player)) - 200) (15 * fromIntegral (snd (playerPosition player)) - 200) (color yellow (circleSolid 5))
