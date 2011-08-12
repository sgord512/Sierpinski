--Filename: maze.hs
--Project: A maze generator and solver
--Author: Spencer Gordon
--Date: August 11th, 2011

module Main where

import Base.Graphics
import Base.Grid
import Data.List ( partition )
import Data.Set ( Set )
import qualified Data.Set as Set
import Graphics.Gloss
import System.Random



yTiles = 10
xTiles = 10

corner = (200,200)

main = do let board = [Tile x y | x <- [-xTiles..xTiles], y <- [-yTiles..yTiles] ]
              rdmGen = mkStdGen 8
              (connections, walls) = buildWalls rdmGen $ connectTiles board
              drawnObjects = Pictures $ (map toPicture board) ++ (map toPicture $ Set.toList connections) ++ (map toPicture $ Set.toList walls)
              sizeFromRowsCols = (\s -> (s + 2) * 2 * (gap + tileRadius) + 2 * gap)
          displayInWindow "Maze Generator by Spencer Gordon"
                          ((ceiling . sizeFromRowsCols . fromIntegral) `onPair` (xTiles, yTiles))
                          corner
                          black
                          drawnObjects

-- A list of all the neighboring tiles to a tile

neighbors :: Tile -> [Tile]
neighbors (Tile x y) = filter (withinBounds (xTiles, yTiles)) [Tile (x + 1) y
                                                              ,Tile x (y + 1)
                                                              ,Tile (x - 1) y
                                                              ,Tile x (y - 1)
                                                              ]
                                                              
-- Checks whether a specified row and column is on the grid

withinBounds :: (Int, Int) -> Tile -> Bool
withinBounds (xLim, yLim) (Tile x y) = ((abs x) <= xLim) && ((abs y) <= yLim)



buildWalls :: StdGen -> Set Connected -> (Set Connected, Set Wall)
buildWalls rdmGen connections = let results = partition (\(a, _) -> a) (zip (randoms rdmGen) (Set.toList connections))
                                    (conn, other) = (Set.fromList . snd . unzip) `onPair` results
                                in (conn, Set.map (\(Connect a b) -> (Block a b)) other)

-- Connects all tiles

connectTiles :: [Tile] -> Set Connected
connectTiles = foldl addConnections Set.empty

-- Takes a set of connections and a tile and adds the connections between that tile and its neighbors to the set

addConnections :: Set Connected -> Tile -> Set Connected
addConnections conn tile = foldr (\neighbor -> Set.insert (Connect tile neighbor)) conn (neighbors tile)
