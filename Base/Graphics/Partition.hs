module Base.Graphics.Partition where

import Base.Graphics
import Base.Grid
import Base.Maze
import Base.Path
import Data.Array ( elems )
import Graphics.Gloss

-- This takes a list of tileGroups and generates a picture for the board as it is partitioned

boardToPartitionPicture :: [TileGroup] -> Board -> Picture
boardToPartitionPicture tileGroups b = let cx = connectrix b
                                           tiles = allTiles b
                                           walls = allWalls b
                                           connections = concat $ elems cx
                                           groupColorMap = zip tileGroups (cycle groupColors)              
                                           coloredWalls = Pictures $ map toPicture walls
                                           coloredConnects = colorConnectionsWithMap connections groupColorMap
                                           coloredTiles = colorTilesWithMap tiles groupColorMap
                                       in Pictures (coloredWalls:coloredConnects:coloredTiles:[])



