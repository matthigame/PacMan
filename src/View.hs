{-# language NamedFieldPuns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gState = let player'         = (player gState)
                      playerPosition' = (playerPosition player')
                  in  pictures [drawMaze (maze gState) playerPosition', drawPlayer player', drawGhosts (ghosts gState) playerPosition', drawPoints (points gState)]

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
drawPlayer player = let nextDirectionArrow = (direcToCoord (nextDirection player))
                    in pictures [(color yellow (circleSolid 5)), translate (8 * (fst nextDirectionArrow)) (8 * (snd nextDirectionArrow)) (color yellow (rectangleWire 5 5))]

direcToCoord :: Direction -> (Float, Float)
direcToCoord North = (0, 1)
direcToCoord East  = (1, 0)
direcToCoord South = (0, -1)
direcToCoord West  = (-1, 0)
                  

drawGhosts :: Ghosts -> Coordinate -> Picture
drawGhosts ghosts playerPosition = cameraTranslate (pictures (map drawGhost ghosts)) playerPosition
  where
    drawGhost :: Ghost -> Picture
    drawGhost ghost = translate (15 * fromIntegral (fst (ghostPosition ghost)) - 200) (15 * fromIntegral (snd (ghostPosition ghost)) - 200) (color red (circleSolid 5))

drawPoints :: Int -> Picture
drawPoints i = translate (-300) (-300) (color white (text (show i)))

cameraTranslate :: Picture -> Coordinate -> Picture
cameraTranslate picture playerPosition = translate (-(15 * fromIntegral (fst playerPosition) - 200)) (-(15 * fromIntegral (snd playerPosition) - 200)) picture