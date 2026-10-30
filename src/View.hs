{-# language NamedFieldPuns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gState = pictures [drawMaze (maze gState), drawPlayer (player gState), drawGhosts (ghosts gState)]

drawMaze :: Maze -> Picture
drawMaze maze = pictures (drawTiles maze (0, 0))
  where
    drawTiles :: Maze -> Coordinate -> [Picture]
    drawTiles _ (-1, -1)        = []
    drawTiles maze coord@(x, y) = translate (15 * fromIntegral x - 200)  (15 * fromIntegral y - 200) (case whatTile maze coord of
      W -> color blue (rectangleSolid 15 15)
      E -> color black (rectangleSolid 15 15)
      P -> color yellow (circleSolid 2)
      S -> color yellow (circleSolid 5)) : drawTiles maze (nextCoordinate maze coord)

    nextCoordinate :: Maze -> Coordinate -> Coordinate
    nextCoordinate maze@(a : as) (x, y) | x + 1 < length a    = (x + 1, y)
                                        | y + 1 < length maze = (0, y + 1)
                                        | otherwise           = (-1, -1)

drawPlayer :: Player -> Picture
drawPlayer player = translate (15 * fromIntegral (fst (playerPosition player)) - 200) (15 * fromIntegral (snd (playerPosition player)) - 200) (color yellow (circleSolid 5))

drawGhosts :: Ghosts -> Picture
drawGhosts ghosts = pictures (map drawGhost ghosts)
  where
    drawGhost :: Ghost -> Picture
    drawGhost ghost = translate (15 * fromIntegral (fst (ghostPosition ghost)) - 200) (15 * fromIntegral (snd (ghostPosition ghost)) - 200) (color red (circleSolid 5))