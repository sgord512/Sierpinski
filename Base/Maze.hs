module Base.Maze where

import Base.Graphics
import Base.Grid
import Control.Monad.State
import Data.Array
import Data.List ( find, mapAccumL, partition, (\\) )
import Data.Maybe ( catMaybes, fromJust, isNothing )
import Data.Tuple
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Random

-- A 2d array of lists of connections. Name kept short at the expense of being self-documenting
-- I store each connection in the coordinates of its index, which should speed up lookup
-- This may not be worth the loss in readibility

type Connectrix = Array (Int, Int) [Connected]
type CxAssoc = ((Int, Int), [Connected])
type BoardState = State (Connectrix)
type XYLimits = ((Int, Int), (Int, Int))
type TileGroup = [Tile]


limitsFromDim :: (Int, Int) -> XYLimits
limitsFromDim dim = let maxPair = (floor . ( / 2) . fromIntegral) `onPair` dim
                        minPair = (negate . ceiling . ( / 2) . fromIntegral) `onPair` dim
                    in (minPair, maxPair)
                                                              
-- Checks whether a specified row and column is on the grid

withinLimits :: XYLimits -> Tile -> Bool
withinLimits ((xMin, yMin), (xMax, yMax)) (Tile x y) = (x <= xMax && x >= xMin && y <= yMax && y >= yMin)

-- Takes all connected tiles and randomly makes walls in place of connections

buildWalls :: StdGen -> Set Connected -> (Set Connected, Set Wall)
buildWalls rdmGen connections = let results = partition (\(a, _) -> a) (zip (randoms rdmGen) (Set.toList connections))
                                    (conn, other) = (Set.fromList . snd . unzip) `onPair` results
                                in (conn, Set.map (\(Connect a b) -> (Block a b)) other)

-- Takes the set of connections that is generated, and creates the connectrix out of them

placeConnectionsInArray :: XYLimits -> Set Connected -> Connectrix
placeConnectionsInArray ((xMin, yMin), (xMax, yMax)) connections = let conn = Set.toList connections
                                                                   in accumArray (\b a -> a ++ b) [] ((xMin, yMin), (xMax, yMax))
                                                                                                  [((x, y), findConnectionForTile (x, y) conn) | x <- [xMin..xMax]
                                                                                                                                               , y <- [yMin..yMax]
                                                                                                                                               , existsConnectionForTile (x, y) conn
                                                                                                                                               ]
-- Given coordinates and a list of connections, returns connections that would have those coordinates in the connectrix                                                                                     

findConnectionForTile :: (Int, Int) -> [Connected] -> [Connected]
findConnectionForTile loc conn = filter (\c -> loc == (coord $ ix c)) conn

-- Given coordinates and a list of connections, determines whether or not there would be any connections at those coordinates

existsConnectionForTile :: (Int, Int) -> [Connected] -> Bool
existsConnectionForTile loc conn = case findConnectionForTile loc conn of
                                        [] -> False
                                        otherwise -> True

getLimits :: BoardState XYLimits
getLimits = do cx <- get
               return $ bounds cx

-- A list of all the neighboring tiles to a tile

neighbors :: Tile -> BoardState TileGroup
neighbors (Tile x y) = do limits <- getLimits 
                          return $ filter (withinLimits limits) [Tile (x + 1) y
                                                                ,Tile x (y + 1)
                                                                ,Tile (x - 1) y
                                                                ,Tile x (y - 1)
                                                                ]
-- Connects all tiles

connectTiles :: TileGroup -> BoardState (Set Connected)
connectTiles = foldM addConnections Set.empty

-- Get all the connections that are at a particular Tile's coordinates

getConn :: Tile -> BoardState [Connected] 
getConn tile = do cx <- get
                  return $ cx ! (coord tile)

-- Adds all possible connections from a tile to the set of connections

addConnections :: Set Connected -> Tile -> BoardState (Set Connected)
addConnections conn tile = do nbors <- neighbors tile
                              return $ foldr (\neighbor -> Set.insert (connect tile neighbor)) conn nbors

-- Removes all tiles in a group from the connectrix and returns the final group and the updated connectrix

removeGroup :: TileGroup -> BoardState TileGroup
removeGroup tiles = do taken <- mapM getAllLinked tiles
                       case concat taken of
                            [] -> return tiles
                            ts -> do moreTiles <- removeGroup ts
                                     return $ tiles ++ moreTiles

-- Find all tiles to add to a group, and updates the connectrix in the process

findConnectedTiles :: Tile -> Connected -> BoardState TileGroup
findConnectedTiles t c = do connections <- getConn (ix c)
                            let (removed, kept) = partition (== c) connections
                            updateConnectrix (coord $ ix c, kept)
                            return (map (\c -> fromJust (c `links` t)) removed)

-- Update the connectrix

updateConnectrix :: CxAssoc -> BoardState ()
updateConnectrix assocs = modify (// [assocs])
                           
-- Gets all tiles that are linked to the given tile, and returns them along with the updated connectrix

getAllLinked :: Tile -> BoardState TileGroup
getAllLinked tile = do pc <- possibleConnections tile
                       linkedTiles <- mapM (\c -> findConnectedTiles tile c) pc
                       return $ concat linkedTiles

-- Returns all possible connections for a tile

possibleConnections :: Tile -> BoardState [Connected]
possibleConnections tile = do nbors <- neighbors tile
                              return $ map (\t -> connect tile t) nbors

-- Takes all connections and splits them into groups by accessibility

findGroups :: TileGroup -> BoardState [TileGroup]
findGroups [] = return []        
findGroups tiles@(t:_) = do group <- removeGroup [t]
                            case (tiles \\ group) of
                               [] -> return (group:[])
                               remaining -> do moreGroups <- findGroups remaining
                                               return (group:moreGroups)

-- An alias for findGroups, used in the executable

partitionBoard = findGroups  

-- Unused helper, conversion from lists of tiles representing groups to sets

groupsToSets :: [TileGroup] -> [Set Tile]
groupsToSets = map Set.fromList

{--
-- Helper, currently unused

onMaybe :: (b -> a -> b) -> Maybe b -> a -> Maybe b
onMaybe f x y = case x of
                   Nothing -> Nothing
                   Just something -> (Just $ f something y)

-- Helper, currently unused; determines whether or not a connection exists in the Connectrix

checkExistence :: Connected -> BoardState Bool
checkExistence c = do cx <- get
                      return $ c `elem` (cx ! (coord $ ix c))
--}