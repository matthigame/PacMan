{-# language NamedFieldPuns #-}
module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game



main :: IO ()
main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              5               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function



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

