module Base.Grid ( Tile (Tile, Start, End), Direction (N, E, S, W), directionFrom, isNS, isEW, tile, Wall ( Block ), Connected ( Connect ), coord, connect, ix, linked, links, onPair) where

-- A grid location, uniquely identified by a column and row

data Tile = Tile Int Int | Start Int Int | End Int Int deriving (Eq, Show)

instance Ord Tile where
         (Tile x y) `compare` (Tile x' y') = case x `compare` x' of
                                                  EQ -> y `compare` y'
                                                  r -> r



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

-- Helper for converting to Tile

tile :: (Int, Int) -> Tile
tile (x, y) = Tile x y

-- A connection between two tiles (valid for movement)

data Wall = Block Tile Tile deriving (Ord, Show)

instance Eq Wall where
         (Block a b) == (Block a' b') = (a == a' && b == b') ||
                                        (a == b' && b == a') 

-- I want to have connections always have put the tiles in an order so that I can cut down on a lot of checks, but this makes it impossible to pattern-match
-- Right now, I am leaving the constructor accessible to other modules, but I don't think that is a good permanent solution

data Connected = Connect Tile Tile deriving (Ord, Show)

coord :: Tile -> (Int, Int)
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



-- Applies a function to both elements of a tuple

onPair :: (a -> b) -> (a, a) -> (b, b)
onPair f (x, y) = (f x, f y)
