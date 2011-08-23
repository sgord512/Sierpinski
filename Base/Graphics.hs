module Base.Graphics where

import Base.Grid
import Base.Maze
import Base.Toolbox ( onPair )
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Data.Array ( elems )
import Data.List ( find )
import Data.Maybe ( fromJust, fromMaybe )

type TileGroupColorMap = [([Tile], Color)] 
       
tileRadius = 20
gap = 3
tileColor = azure
wallColor = white
groupColors = [red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]
corner = (200,200) :: (Int, Int)
failedDjikstraColor = red
passedDjikstraColor = green
 
-- A typeclass of all things that can be drawn

class Visible a where
      toPicture :: a -> Picture

-- Returns the picture representation of a tile

instance Visible Tile where
{--         toPicture (Tile x y) | (x == 0 && y == 0) = let coord = bottomLeftCorner (fromIntegral `onPair` (x, y))
                                                     in (Color cyan (square coord tileRadius))
--}                                                  
         toPicture (Tile x y) | otherwise = let coord = bottomLeftCorner (fromIntegral `onPair` (x, y))
                                            in (square coord tileRadius)
{--         
         toPicture (End x y) = let coord = bottomLeftCorner (fromIntegral `onPair` (x, y))
                                   (centerX, centerY) = (+ (tileRadius /2)) `onPair` (bottomLeftCorner (fromIntegral `onPair` (x, y)))
                               in Pictures ((toPicture (Tile x y)):(Translate centerX centerY $ Color black $ circleSolid (tileRadius / 2)):[])
--}
-- Given a list of Tile Groups and corresponding colors and a tile, returns the color that tile should have

findGroupColor :: TileGroupColorMap -> Tile -> Color
findGroupColor groupColorMap tile = snd (fromMaybe ([], tileColor) $ (find (\(group, _) -> tile `elem` group) groupColorMap))

colorTilesWithMap :: TileGroup -> TileGroupColorMap -> Picture
colorTilesWithMap tiles groupColorMap = Pictures $ map (\(tile, pic) -> Color (findGroupColor groupColorMap tile) pic) (zip tiles (map toPicture tiles))

colorConnectionsWithMap :: [Connected] -> TileGroupColorMap -> Picture
colorConnectionsWithMap connections groupColorMap = let connPicMap = zip connections (map toPicture connections)
                                                        computeColor = (\(Connect a b) -> let (c, c') = (findGroupColor groupColorMap) `onPair` (a, b)
                                                                                          in if c == c' then c else tileColor)
                                                    in Pictures $ map (\(connection, pic) -> Color (computeColor connection) pic) connPicMap

-- Draws a square of a given side-length from a row and col held as a tile

square point length = rect point length length

rect (x, y) w h = Polygon [(x, y)
                          ,(x, y + h)
                          ,(x + w, y + h)
                          ,(x + w, y)
                          ]

-- Calculates the Bottom Left corner of a tile in a given row and column

bottomLeftCorner (x, y) = (subtract 0) `onPair` (x * (gap + tileRadius) + gap
                                                ,y * (gap + tileRadius) + gap
                                                )

-- Returns the picture representation of a wall between two tiles

instance Visible Connected where
         toPicture (Connect a@(Tile x y) b@(Tile x' y')) = let dir = (fromIntegral `onPair` (x, y)) `directionFrom` (fromIntegral `onPair` (x', y'))
                                                               o = bottomLeftCorner (fromIntegral `onPair` (x', y'))
                                                               w = if isNS dir then tileRadius else gap
                                                               h = if isNS dir then gap else tileRadius
                                                           in (rect (o `goTo` dir) w h)

instance Visible Wall where
         toPicture (Block a@(Tile x y) b@(Tile x' y')) = let dir = (fromIntegral `onPair` (x, y)) `directionFrom` (fromIntegral `onPair` (x', y'))
                                                             o = bottomLeftCorner (fromIntegral `onPair` (x', y'))
                                                             w = if isNS dir then tileRadius else gap
                                                             h = if isNS dir then gap else tileRadius
                                                         in Color wallColor (rect (o `goTo` dir) w h)
 



goTo :: (Float, Float) -> Direction -> (Float, Float)
goTo (x, y) dir = case dir of
                       N -> (x, y + tileRadius)
                       E -> (x + tileRadius, y)
                       W -> (x - gap, y)
                       S -> (x, y - gap)

-- This generates an uncolored picture of the board

boardToPlainPicture :: Board -> Picture
boardToPlainPicture b = let tiles = allTiles b
                            walls = allWalls b
                            cx = connectrix b
                            connections = concat $ elems cx
                            coloredWalls = Pictures $ map toPicture walls
                            coloredConnects = Pictures $ map (\c -> Color tileColor (toPicture c)) connections                                                                  
                            coloredTiles = Pictures $ map (\t -> Color tileColor (toPicture t)) tiles
                        in Pictures (coloredWalls:coloredConnects:coloredTiles:[])