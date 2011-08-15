--Filename: maze.hs
--Project: A maze generator and solver
--Author: Spencer Gordon
--Date: August 11th, 2011

module Main where

import Base.Graphics
import Base.Grid
import Base.Maze
import qualified Data.Set as Set
import Graphics.Gloss
import System.Random

groupColors = [red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]

corner = (200,200)

main = do putStrLn "Hello, World"
          rdmGen <- getStdGen
          let board = [Tile x y | x <- [-xTiles..xTiles], y <- [-yTiles..yTiles] ]
              (connections, walls) = buildWalls rdmGen $ connectTiles board
              groupColorMap = zip (groupsToSets $ (partitionBoard board $ placeConnectionsInArray connections)) (cycle groupColors)
              coloredWalls = map toPicture $ Set.toList walls
              coloredConnects = map (\(Connect a b, pic) -> let (c, c') = (findGroupColor groupColorMap) `onPair` (a, b)
                                                                color = if (c == c') then c else black
                                                            in Color color pic)
                                    (zip (Set.toList connections) (map toPicture $ Set.toList connections))
              coloredTiles = map (\(tile, pic) -> Color (findGroupColor groupColorMap tile) pic) (zip board (map toPicture board))
              drawnObjects = Pictures (coloredWalls ++ coloredConnects ++ coloredTiles) 
              sizeFromRowsCols = (\s -> (s + 2) * 2 * (gap + tileRadius) + 2 * gap)
          displayInWindow "Maze Generator by Spencer Gordon"
                          ((ceiling . sizeFromRowsCols . fromIntegral) `onPair` (xTiles, yTiles))
                          corner
                          black
                          drawnObjects



