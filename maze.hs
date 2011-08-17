--Filename: maze.hs
--Project: A maze generator and solver
--Author: Spencer Gordon
--Date: August 11th, 2011

module Main where

import Base.Graphics
import Base.Grid
import Base.Maze
import Control.Monad.State
import Data.Array ( array )
import qualified Data.Set as Set
import Graphics.Gloss hiding ( dim )
import System
import System.Console.GetOpt
import System.Random


groupColors = [red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]

corner = (200,200)

main = do putStrLn "Hello, World"
          input <- getArgs
          let (opts, nonOpts, err) = getOpt Permute options input
              dim = case opts of
                         [] -> (21, 21)
                         int:[] -> (int, int)
          rdmGen <- getStdGen
          let tiles = generateTiles dim
              mockState = array (limitsFromDim dim) []
              (connections, walls) = buildWalls rdmGen $ (evalState (connectTiles tiles) mockState)
              (tileGroups, board) = runState (partitionBoard tiles) (placeConnectionsInArray (limitsFromDim dim) connections)            
              groupColorMap = zip (groupsToSets tileGroups) (cycle groupColors)
              coloredWalls = map toPicture $ Set.toList walls
              coloredConnects = map (\(Connect a b, pic) -> let (c, c') = (findGroupColor groupColorMap) `onPair` (a, b)
                                                                color = if (c == c') then c else black
                                                            in Color color pic)
                                    (zip (Set.toList connections) (map toPicture $ Set.toList connections))
              coloredTiles = map (\(tile, pic) -> Color (findGroupColor groupColorMap tile) pic) (zip tiles (map toPicture tiles))
              drawnObjects = Pictures (coloredWalls ++ coloredConnects ++ coloredTiles) 
              sizeFromRowsCols = (\s -> (s + 2) * (gap + tileRadius) + 2 * gap)
          displayInWindow "Maze Generator by Spencer Gordon"
                          ((ceiling . sizeFromRowsCols . fromIntegral) `onPair` dim)
                          corner
                          black
                          drawnObjects


options = [ Option ['s'] ["size"] (ReqArg read "SIDE LENGTH OF SQUARE") "Choose how large a maze you would like to create" 
          ]


generateTiles :: (Int, Int) -> [Tile]
generateTiles dim = let ((xMin, yMin), (xMax, yMax)) = limitsFromDim dim
                    in [Tile x y | x <- [xMin..xMax], y <- [yMin..yMax] ]          

