module Base.Maze where

import Base.Graphics
import Base.Grid
import Data.List ( partition, mapAccumL )
import Data.Maybe ( catMaybes, fromJust, isNothing )
import Data.Tuple
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Random

yTiles = 10
xTiles = 10

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

-- Takes all connected tiles and randomly makes walls in place of connections

buildWalls :: StdGen -> Set Connected -> (Set Connected, Set Wall)
buildWalls rdmGen connections = let results = partition (\(a, _) -> a) (zip (randoms rdmGen) (Set.toList connections))
                                    (conn, other) = (Set.fromList . snd . unzip) `onPair` results
                                in (conn, Set.map (\(Connect a b) -> (Block a b)) other)

-- Connects all tiles

connectTiles :: [Tile] -> Set Connected
connectTiles = foldl addConnections Set.empty

-- Takes a set of connections and a tile and adds the connections between that tile and its neighbors to the set


type TileGroup = [Tile]

addConnections :: Set Connected -> Tile -> Set Connected
addConnections conn tile = foldr (\neighbor -> Set.insert (Connect tile neighbor)) conn (neighbors tile)

-- Takes all connections and splits them into groups by accessibility

removeGroup :: [Tile] -> [Connected] -> ([Tile], [Connected])
removeGroup tiles conns = let (remaining, taken) = mapAccumL removeAllLinked conns tiles
                          in case (concat taken) of
                                  [] -> (tiles, remaining)
                                  ts -> let (moreTiles, moreRemaining) = removeGroup ts remaining
                                        in (tiles ++ moreTiles, moreRemaining)
                                 
getAllLinked :: Tile -> [Connected] -> [Tile]
getAllLinked tile connections = catMaybes $ map (`links` tile) connections        

removeAllLinked :: [Connected] -> Tile -> ([Connected], [Tile])
removeAllLinked connections tile = let connectionsZipped = zip (map (`links` tile) connections) connections
                                       part = (\(t, _) -> isNothing t)
                                       (conn, tiles) = partition part connectionsZipped
                                   in (map (\(_, c) -> c) conn, map (\(t, _) -> fromJust t) tiles)

findSingletons :: [Tile] -> [[Tile]] -> [Tile]
findSingletons allTiles tileGroups = filter (`notElem` concat tileGroups) allTiles

findGroups :: [Connected] -> [[Tile]]
findGroups [] = []        
findGroups c@((Connect a b):cs) = let (g, r) = removeGroup [a,b] c
                    in case r of
                         [] -> g:[]
                         remaining -> (g : (findGroups remaining))

partitionBoard :: [Tile] -> [Connected] -> [[Tile]]
partitionBoard allTiles allConnections = let allGroups = findGroups allConnections
                                         in allGroups ++ (map (\t -> [t]) $ findSingletons allTiles allGroups)


groupsToSets :: [[Tile]] -> [Set Tile]
groupsToSets = map Set.fromList

