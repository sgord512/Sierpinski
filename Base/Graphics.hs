module Base.Graphics where

import Base.Grid
import Graphics.Gloss
import Data.List ( find )
import Data.Maybe ( fromJust, fromMaybe )
import Data.Set ( Set )
import qualified Data.Set as Set

       
tileRadius = 20
gap = 3
tileColor = azure
wallColor = white
 
-- A typeclass of all things that can be drawn

class Visible a where
      toPicture :: a -> Picture

-- Returns the picture representation of a tile

instance Visible Tile where
         toPicture (Tile x y) = let coord = bottomLeftCorner (fromIntegral `onPair` (x, y))
                                in (square coord tileRadius)

findGroupColor :: [(Set Tile, Color)] -> Tile -> Color
findGroupColor groupColorMap tile = snd (fromMaybe (Set.empty, black) $ (find (\(set, _) -> tile `Set.member` set) groupColorMap))

-- Draws a square of a given side-length from a row and col held as a tile

square point length = rect point length length

rect (x, y) w h = Polygon [(x, y)
                          ,(x, y + h)
                          ,(x + w, y + h)
                          ,(x + w, y)
                          ]

-- Calculates the Bottom Left corner of a tile in a given row and column

bottomLeftCorner (x, y) = (\a -> a - ((gap + tileRadius) / 2)) `onPair` (x * (gap + tileRadius) + gap
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
