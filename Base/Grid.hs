module Base.Grid where

import Base.Toolbox ( onPair )

type Point = (Int, Int)
type XYLimits = (Point, Point)
type TileGroup = [Tile]

-- Used to make drawing easier

data Direction = N | E | S | W deriving (Eq, Ord, Enum)

directionFrom :: (Float, Float) -> (Float, Float) -> Direction
directionFrom (x, y) (x', y') = case (x `compare` x', y `compare` y') of
                                     (GT, EQ) -> E
                                     (LT, EQ) -> W
                                     (EQ, GT) -> N
                                     (EQ, LT) -> S

isNS :: Direction -> Bool
isNS d = (d == N || d == S)

isEW :: Direction -> Bool
isEW d = (d == E || d == W)

-- A grid location, uniquely identified by a column and row

data Tile = Tile Int Int deriving (Show)

instance Eq Tile where
         (Tile x y) == (Tile x' y') = (x == x') && (y == y')

instance Ord Tile where
         (Tile x y) `compare` (Tile x' y') = let result =  x `compare` x'
                                             in case result of
                                                  EQ -> y `compare` y'
                                                  otherwise -> result
         
normalize :: Tile -> Tile
normalize t = t


-- Helper for converting to Tile

tile :: Point -> Tile
tile (x, y) = Tile x y

-- A wall between two tiles, invalid for movement

data Wall = Block Tile Tile deriving (Ord, Show)

instance Eq Wall where
         (Block a b) == (Block a' b') = (a == a' && b == b') ||
                                        (a == b' && b == a') 

-- I want to have connections always have put the tiles in an order so that I can cut down on a lot of checks, but this makes it impossible to pattern-match
-- Right now, I am leaving the constructor accessible to other modules, but I don't think that is a good permanent solution

-- A connection between two tiles (valid for movement)

data Connected = Connect Tile Tile deriving (Ord, Show)

coord :: Tile -> Point
coord (Tile x y) = (x, y)

connect :: Tile -> Tile -> Connected
connect a b = case a `compare` b of
                   LT -> Connect a b
                   GT -> Connect b a
                   _  -> error "Cannot connect a tile to itself"      

ix :: Connected -> Tile
ix (Connect a b) = case a `compare` b of
                        LT -> a
                        GT -> b
                        _  -> error "This connection is malformed"

instance Eq Connected where
         (Connect a b) == (Connect a' b') = (a == a' && b == b')
                                            

linked :: Connected -> Connected -> Bool
linked (Connect a b) (Connect a' b') = case a `compare` a' of
                                            GT -> a == b'
                                            LT -> False
                                            EQ -> True

links :: Connected -> Tile -> Maybe Tile
links (Connect a b) t | (a == t) = Just b
                      | (b == t) = Just a
                      | otherwise = Nothing

-- Calculates the rows and columns that are valid given a width and height

limitsFromDim :: Point -> XYLimits
limitsFromDim dim = let maxPair = (floor . ( / 2) . fromIntegral) `onPair` dim
                        minPair = (negate . ceiling . ( / 2) . fromIntegral) `onPair` dim
                    in (minPair, maxPair)

-- Generates all tiles within in a grid of given dimensions
                                                              
generateTiles :: Point -> [Tile]
generateTiles dim = let ((xMin, yMin), (xMax, yMax)) = limitsFromDim dim
                    in [Tile x y | x <- [xMin..xMax], y <- [yMin..yMax] ]          

-- Checks whether a specified row and column is on the grid

withinLimits :: XYLimits -> Tile -> Bool
withinLimits ((xMin, yMin), (xMax, yMax)) (Tile x y) = (x <= xMax && x >= xMin && y <= yMax && y >= yMin)

-- Checks whether a specified tile is on the edge of the grid

isEdgeTile :: XYLimits -> Tile -> Bool
isEdgeTile ((xMin, yMin), (xMax, yMax)) (Tile x y) = (x == xMax || x == xMin || y == yMax || y == yMin)

{-- Helper, currently unused; determines whether or not a connection exists in the Connectrix

checkExistence :: Connected -> BoardState Bool
checkExistence c = do cx <- getCx
                      return $ c `elem` (cx ! (coord $ ix c))

--}