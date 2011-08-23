module Base.Maze where

import Base.Grid
import Base.Toolbox ( onPair )
import Control.Monad.State
import Data.Array
import Data.List ( delete, find, mapAccumL, partition, (\\), insert )
import Data.Maybe ( catMaybes, fromJust, isNothing )
import Data.Tuple
import Debug.Trace
import System.Random

-- A 2d array of lists of connections. Name kept short at the expense of being self-documenting
-- I store each connection in the coordinates of its index, which should speed up lookup
-- This may not be worth the loss in readibility

type Connectrix = Array Point [Connected]
type CxAssoc = (Point, [Connected])
type BoardState = State Board

-- All the state that I want to store for the game. I use record syntax so I don't have to change anything to add new state

data Board = Board
            { allTiles :: TileGroup
            , connectrix :: Connectrix
            , allWalls :: [Wall]
            } deriving ( Show )


setUpMaze :: Point -> StdGen -> Board
setUpMaze dim rdm = let tiles = generateTiles dim
                        c = connectAllTiles (limitsFromDim dim) tiles
                        (connections, builtWalls) = buildWalls rdm c
                        cx = placeConnectionsInArray (limitsFromDim dim) connections
                    in Board { allTiles = tiles
                             , connectrix = cx
                             , allWalls = builtWalls
                             }

pickStartAndEnd :: StdGen -> BoardState StartEnd
pickStartAndEnd rdm = do tiles <- getTiles
                         limits <- getLimits
                         let (edgeTiles, innerTiles) = partition (isEdgeTile limits) tiles
                             (startIx, rdm') = ((randomR (0, (subtract 1) (length edgeTiles)) rdm) :: (Int, StdGen))
                             (start, edgeTiles') = ((edgeTiles !! startIx), (take ((subtract 1) startIx) edgeTiles) ++ (drop startIx edgeTiles))
                             (endIx, _) = ((randomR (0, (subtract 1) (length edgeTiles')) rdm') :: (Int, StdGen))
                             end = (edgeTiles' !! endIx)
                         return $ (start, end)
                      



-- Takes all connected tiles and randomly makes walls in place of connections

buildWalls :: StdGen -> [Connected] -> ([Connected], [Wall])
buildWalls rdmGen connections = let results = partition (\(a, _) -> a) (zip (randoms rdmGen) connections)
                                    (conn, other) = (snd . unzip) `onPair` results
                                in (conn, map (\(Connect a b) -> (Block a b)) other)

-- Takes the set of connections that is generated, and creates the connectrix out of them

placeConnectionsInArray :: XYLimits -> [Connected] -> Connectrix
placeConnectionsInArray ((xMin, yMin), (xMax, yMax)) conn = let 
                                                            in accumArray (\b a -> a ++ b) [] ((xMin, yMin), (xMax, yMax))
                                                                                              [((x, y), findConnectionForTile (x, y) conn) | x <- [xMin..xMax]
                                                                                                                                           , y <- [yMin..yMax]
                                                                                                                                           , existsConnectionForTile (x, y) conn
                                                                                                                                           ]

-- Given coordinates and a list of connections, returns connections that would have those coordinates in the connectrix                                                                                     

findConnectionForTile :: Point -> [Connected] -> [Connected]
findConnectionForTile loc conn = filter (\c -> loc == (coord $ ix c)) conn

-- Given coordinates and a list of connections, determines whether or not there would be any connections at those coordinates

existsConnectionForTile :: Point -> [Connected] -> Bool
existsConnectionForTile loc conn = case findConnectionForTile loc conn of
                                        [] -> False
                                        otherwise -> True

-- START OF FUNCTIONS IN THE BOARDSTATE MONAD

getLimits :: BoardState XYLimits
getLimits = do board <- get
               return $ bounds $ connectrix board

getCx :: BoardState Connectrix
getCx = do board <- get
           return $ connectrix board

putCx :: Connectrix -> BoardState ()
putCx cx = do board <- get
              put $ board { connectrix = cx }

getTiles :: BoardState TileGroup
getTiles = do board <- get
              return $ allTiles board

putTiles :: TileGroup -> BoardState ()
putTiles tiles = do board <- get
                    put $ board { allTiles = tiles }

getWalls :: BoardState [Wall]
getWalls = do board <- get
              return $ allWalls board


-- A list of all the neighboring tiles to a tile

neighbors :: XYLimits -> Tile -> TileGroup
neighbors limits (Tile x y) = filter (withinLimits limits) [Tile (x + 1) y
                                                           ,Tile x (y + 1)
                                                           ,Tile (x - 1) y
                                                           ,Tile x (y - 1)
                                                           ]

-- Connects all tiles

connectAllTiles :: XYLimits -> TileGroup -> [Connected]
connectAllTiles limits tiles = foldl (addConnections limits) [] tiles

-- Get all the connections that are at a particular Tile's coordinates

getConn :: Tile -> BoardState [Connected] 
getConn tile = do cx <- getCx
                  return $ cx ! (coord tile)

-- Adds all possible connections from a tile to the set of connections

addConnections :: XYLimits -> [Connected] -> Tile -> [Connected]
addConnections limits conn tile = let nbors = neighbors limits tile
                                  in foldl (\connList neighbor -> let connection = (connect tile neighbor)
                                                                  in if connection `elem` connList
                                                                        then connList
                                                                        else (insert connection connList)) conn nbors

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
updateConnectrix assocs = do cx <- getCx
                             let cx' = cx // [assocs]
                             putCx cx'
                           
-- Gets all tiles that are linked to the given tile, and returns them along with the updated connectrix

getAllLinked :: Tile -> BoardState TileGroup
getAllLinked tile = do pc <- possibleConnections tile
                       linkedTiles <- mapM (\c -> findConnectedTiles tile c) pc
                       return $ concat linkedTiles

-- Returns all possible connections for a tile

possibleConnections :: Tile -> BoardState [Connected]
possibleConnections tile = do limits <- getLimits
                              let nbors = neighbors limits tile
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

partitionBoard :: BoardState [TileGroup]
partitionBoard = do tiles <- getTiles 
                    findGroups tiles


