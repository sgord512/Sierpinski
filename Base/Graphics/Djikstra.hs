module Base.Graphics.Djikstra where

import Base.Graphics
import Base.Grid
import Base.Maze
import Base.Path
import Data.Array ( elems )
import Data.List ( (\\) )
import qualified Data.Set as Set
import Debug.Trace
import Graphics.Gloss


-- This generates a picture of the board with the successful path in green or the failed path in red; BROKEN RIGHT NOW

boardToDjikstraPicture :: DjikstraResult -> Board -> Picture
boardToDjikstraPicture result b = let cx = connectrix b
                                      tiles = allTiles b
                                      walls = allWalls b
                                      connections = concat $ elems cx
                                      (highlightColor, group) = case result of
                                                                      Left tilePath -> trace "inspecting djikstra result: success" (passedDjikstraColor, pathToGroup tilePath)
                                                                      Right tileGroup -> trace "inspecting djikstra result: failure"  (failedDjikstraColor, Set.toList tileGroup)
                                      groupColorMap = (group, highlightColor):[]
                                      coloredWalls = Pictures $ map toPicture walls
                                      coloredConnects = colorConnectionsWithMap connections groupColorMap
                                      coloredTiles = colorTilesWithMap tiles groupColorMap
                                  in Pictures (coloredWalls:coloredConnects:coloredTiles:[])


djikstraStatePicture :: Djikstra -> SearchState -> Picture
djikstraStatePicture dk ss = let b = board dk
                                 cx = connectrix b
                                 tiles = allTiles b
                                 walls = allWalls b
                                 visited = Set.toList $ visitedTileSet ss
                                 currentPathTiles = pathToGroup $ currentTilePath ss           
                                 inactiveTiles = visited \\ currentPathTiles
                                 connections = concat $ elems cx
                                 groupColorMap = (currentPathTiles, passedDjikstraColor):(inactiveTiles, failedDjikstraColor):[]
                                 coloredWalls = Pictures $ map toPicture walls
                                 coloredConnects = colorConnectionsWithMap connections groupColorMap
                                 coloredTiles = colorTilesWithMap tiles groupColorMap
                             in Pictures (coloredWalls:coloredConnects:coloredTiles:[])
