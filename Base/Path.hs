module Base.Path where

import Base.Graphics
import Base.Grid
import Base.Maze
import Base.Toolbox ( insertWithoutReplacement, tracing )
import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Array ( bounds, (!) )
import Data.List ( find, partition )
import Data.Maybe ( fromMaybe, fromJust, isNothing )
import Data.Set ( Set )
import qualified Data.Set as Set
import Debug.Trace

data TilePath = Path Tile TilePath Int | Root Tile deriving ( Show )

-- constructs path with correct length

makePath :: Tile -> TilePath -> TilePath
makePath t tp = Path t tp ((pathLength tp) + 1)

-- tilePaths are considered equal if the most recent tile is the same

instance Eq TilePath where
         (Root t) == (Path t' tp _) = t == t'
         (Path t tp _) == (Path t' tp' _) = t == t'
         (Path t tp _) == (Root t') = t == t'

-- tilePaths are ordered by length, and then by tile

instance Ord TilePath where 
         (Root t) `compare` (Root t') = t `compare` t'
         (Path t tp len) `compare` (Path t' tp' len') = case len `compare` len' of
                                                             EQ -> case t `compare` t' of
                                                                        EQ -> tp `compare` tp'
                                                                        res -> res
                                                             res -> res

pathLength :: TilePath -> Int
pathLength (Root t) = 1
pathLength (Path t tp len) = len

-- the Tile at the end of the path

lastTile :: TilePath -> Tile
lastTile (Path t tp _) = t
lastTile (Root t) = t

-- converts a path to a tileGroup for coloring purposes

pathToGroup :: TilePath -> TileGroup
pathToGroup (Path t tp _) = t:(pathToGroup tp)
pathToGroup (Root t) = t:[]

-- unused, checks whether a tile is in a certain path

contains :: Tile -> TilePath -> Bool
contains t tp = case tp of
                  (Root t') -> t == t'
                  (Path t' tp' _) -> t == t' || contains t tp' 

-- the state Monad for Djikstra's Algorithm

type DjikstraState = State Djikstra
type DjikstraResult =  Either TilePath (Set Tile) 

-- I use record syntax to allow modification without disrupting access

data Djikstra = Djikstra { visitedTiles :: Set Tile
                         , pathsToVisit :: Set TilePath
                         , board :: Board
                         , start :: Tile
                         , end :: Tile
                         } deriving ( Show )

-- The actual algorithm (note that it doesn't have any input)

searchForPath :: DjikstraState DjikstraResult
searchForPath = do start <- getStart
                   result <- visitTile (Root start) 
                   case result of 
                        Nothing -> do visited <- getVisited
                                      return $ Right visited
                        Just tp -> return $ Left tp

-- creates the Djikstra to store all the state

setUpDjikstra :: StartEnd -> Board ->  Djikstra
setUpDjikstra (s, e) b = Djikstra { visitedTiles = Set.empty
                                  , pathsToVisit = Set.empty
                                  , board = b
                                  , start = s 
                                  , end = e 
                                  }

getVisited :: DjikstraState (Set Tile)
getVisited = do dk <- get                
                (return $! visitedTiles dk) 

getPathsToVisit :: DjikstraState (Set TilePath)
getPathsToVisit = do dk <- get
                     (return $! pathsToVisit dk) 

-- gets the minimum path in the set of paths to visit or nothing if it is empty, in which case the algorithm should terminate

getNextToVisit :: DjikstraState (Maybe TilePath)
getNextToVisit = do dk <- get
                    ptv <- getPathsToVisit
                    if Set.null ptv then return Nothing
                                    else do let (next, ptv') = (Set.deleteFindMin ptv)
                                            put $ dk { pathsToVisit = ptv' }                                            
                                            return $ Just next


getEnd :: DjikstraState Tile
getEnd = do dk <- get
            return $ end dk

isEnd :: Tile -> DjikstraState Bool
isEnd t = do e <- getEnd
             return $ t == e
             
getStart :: DjikstraState Tile
getStart = do dk <- get
              return $ start dk

isStart :: Tile -> DjikstraState Bool
isStart t = do s <- getStart
               return $ t == s
             
getBoard :: DjikstraState Board
getBoard = do dk <- get
              return $ board dk       

-- encapsulates the fact that neighbors needs the limits of the board

getNeighbors :: Tile -> DjikstraState TileGroup
getNeighbors t = do board <- getBoard
                    let limits = bounds $ connectrix board
                    return $ neighbors limits t

-- this should be implemented as a method of Board outside the state Monad

getReachableNeighbors :: Tile -> DjikstraState TileGroup
getReachableNeighbors t = do nbors <- getNeighbors t
                             board <- getBoard
                             let cx = connectrix board
                                 conns = map (\t' -> (connect t t')) nbors
                                 connected = filter (\c -> c `elem` (cx ! coord (ix c))) conns
                             return $! map (\c -> fromJust $ c `links` t) connected

-- adds the tilePaths created by adding the tiles in the group to the path to the list of pathsToVisit

addToPathsToVisit :: TilePath -> TileGroup -> DjikstraState ()
addToPathsToVisit path tiles = do dk <- get
                                  visited <- getVisited
                                  ptv <- getPathsToVisit
                                  let (tiles', _) = partition (\t -> Set.notMember t visited) tiles
                                      paths = map (\t -> makePath t path) tiles'                                 
                                      ptv' = foldr insertWithoutReplacement ptv paths 
                                  put $ dk { pathsToVisit = ptv' }
                                  (return ())

-- adds a tile to the set of visited tiles

addToVisited :: TileGroup -> DjikstraState ()
addToVisited tiles = do dk <- get
                        visited <- getVisited
                        let visited' = foldr insertWithoutReplacement visited tiles
                        put $ dk { visitedTiles = visited' }
                        (return ())

-- the body of the algorithm, checks if at destination, gets next nodes, updates visited and pathsToVisited, and recurs on nextToVisit

visitTile :: TilePath -> DjikstraState (Maybe TilePath)
visitTile t = do addToVisited ((lastTile t):[])
                 done <- isEnd $ lastTile t
                 if done
                   then return $ Just t
                   else do nbors <- getReachableNeighbors $ lastTile t
                           addToPathsToVisit t nbors
                           next <- getNextToVisit
                           case next of 
                             Nothing -> return Nothing
                             Just tp -> do visitTile tp

-- The WriterT Monad transformer stacked on top of DjikstraState

type RecordDjikstra = WriterT SearchStateList DjikstraState
type SearchStateList = [SearchState]
type SearchState = (TilePath, Set Tile)

currentTilePath :: SearchState -> TilePath
currentTilePath = fst 

visitedTileSet :: SearchState -> Set Tile
visitedTileSet = snd

generateDjikstraList :: Djikstra -> (DjikstraResult, SearchStateList)
generateDjikstraList startingDjikstra = evalState (runWriterT searchForPathAndRecord) startingDjikstra

-- The logging versions of the Djikstra Algorithm
                                           
searchForPathAndRecord :: RecordDjikstra DjikstraResult
searchForPathAndRecord = do start <- lift getStart
                            result <- visitTileAndRecord (Root start)
                            case result of 
                              Nothing -> do visited <- lift getVisited
                                            return $ Right visited
                              Just tp -> return $ Left tp

visitTileAndRecord :: TilePath -> RecordDjikstra (Maybe TilePath)
visitTileAndRecord t = do lift (addToVisited ((lastTile t):[]))
                          visited <- lift getVisited
                          tell [(t, visited)]
                          done <- lift (isEnd $ lastTile t)
                          if done
                            then return $ Just t
                            else do nbors <- lift (getReachableNeighbors $ lastTile t)
                                    lift (addToPathsToVisit t nbors)
                                    next <- lift getNextToVisit
                                    case next of 
                                      Nothing -> return Nothing
                                      Just tp -> do visitTileAndRecord tp
