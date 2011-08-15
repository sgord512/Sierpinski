module Base.Maze where

import Base.Graphics
import Base.Grid
import Data.Array
import Data.List ( find, mapAccumL, partition, (\\) )
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

-- A 2d array of Maybe Connected. Name kept short at the expense of being self-documenting

type Connectrix = Array (Int, Int) [Connected]

-- Get all the connections that are at a particular Tile's coordinates

getConn :: Connectrix -> Tile -> [Connected]
getConn cx tile = cx ! (coord tile)

-- Helper, currently unused

onMaybe :: (b -> a -> b) -> Maybe b -> a -> Maybe b
onMaybe f x y = case x of
                   Nothing -> Nothing
                   Just something -> (Just $ f something y)

-- Helper, currently unused; determines whether or not a connection exists in the Connectrix

checkExistence :: Connectrix -> Connected -> Bool
checkExistence cx c = c `elem` (cx ! (coord $ ix c))

-- Takes the set of connections that is generated, and creates the connectrix out of them

placeConnectionsInArray :: Set Connected -> Connectrix
placeConnectionsInArray connections = let conn = Set.toList connections
                                      in accumArray (\b a -> a ++ b) [] ((-xTiles,-yTiles), (xTiles, yTiles))
                                                                        [((x, y), findConnectionForTile (x, y) conn) | x <- [-xTiles..xTiles]
                                                                                                                     , y <- [-yTiles..yTiles]
                                                                                                                     , existsConnectionForTile (x, y) conn
                                                                                                                     ]

-- Given coordinates and a list of connections, returns connections that would have those coordinates in the connectrix                                                                                     

findConnectionForTile :: (Int, Int) -> [Connected] -> [Connected]
findConnectionForTile loc conn = filter (\c -> loc == (coord $ ix c)) conn

-- Given coordinates and a list of connections, determines whether or not there would be any connections at those coordinates

existsConnectionForTile :: (Int, Int) -> [Connected] -> Bool
existsConnectionForTile loc conn = case filter (\c -> loc == (coord $ ix c)) conn of
                                        [] -> False
                                        otherwise -> True

-- Adds all possible connections from a tile to the set of connections

addConnections :: Set Connected -> Tile -> Set Connected
addConnections conn tile = foldr (\neighbor -> Set.insert (connect tile neighbor)) conn (neighbors tile)

-- Removes all tiles in a group from the connectrix and returns the final group and the updated connectrix

removeGroup :: [Tile] -> Connectrix -> ([Tile], Connectrix)
removeGroup tiles connx = let (cx, taken) = mapAccumL getAllLinkedAndUpdate connx tiles
                          in case concat taken of
                                  [] -> (tiles, cx)
                                  ts -> let (moreTiles, connectrix) = (removeGroup ts cx)
                                        in (tiles ++ moreTiles, connectrix)

-- Find all tiles to add to a group, and return the associations that are used to update the connectrix


findConnectedTiles :: Tile -> Connected -> Connectrix -> ([Tile], ((Int, Int), [Connected]))
findConnectedTiles t c cx = let i = ix c 
                                f = (== c)
                                (removed, kept) = partition f (cx `getConn` i)
                            in (map (\c -> fromJust (c `links` t)) removed, (coord i, kept))

-- Update the connectrix

updateConnectrix :: [ ((Int, Int), [Connected]) ] -> Connectrix -> Connectrix 
updateConnectrix assocs cx = cx // assocs
                           
-- Gets all tiles that are linked to the given tile, and returns them along with the updated connectrix

getAllLinkedAndUpdate :: Connectrix -> Tile -> (Connectrix, [Tile])
getAllLinkedAndUpdate cx tile = let pc = possibleConnections tile
                                    results = map (\c -> findConnectedTiles tile c cx) pc
                                    tiles = concat $ map (\(a, _) -> a) results
                                    assocs = map (\(_, a) -> a) results
                                    cx' = updateConnectrix assocs cx
                                in (cx', tiles)

-- Returns all possible connections for a tile

possibleConnections :: Tile -> [Connected]
possibleConnections tile = map (\t -> connect tile t) (neighbors tile)

-- Takes all connections and splits them into groups by accessibility

findGroups :: [Tile] -> Connectrix -> [[Tile]]
findGroups [] _ = []        
findGroups tiles@(t:_) connx = let (g, cx) = removeGroup [t] connx
                               in case (tiles \\ g) of
                                  [] -> g:[]
                                  remaining -> (g : (findGroups remaining cx))

-- An alias for findGroups, used in the executable

partitionBoard allTiles cx = findGroups allTiles cx

-- Unused helper, conversion from lists of tiles representing groups to sets

groupsToSets :: [[Tile]] -> [Set Tile]
groupsToSets = map Set.fromList