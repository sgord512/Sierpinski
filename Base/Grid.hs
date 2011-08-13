module Base.Grid where

-- A grid location, uniquely identified by a column and row

data Tile = Tile Int Int | Start Int Int | End Int Int deriving (Eq, Ord, Show)


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

data Wall = Block Tile Tile deriving Ord

instance Eq Wall where
         (Block a b) == (Block a' b') = (a == a' && b == b') ||
                                        (a == b' && b == a') 

data Connected = Connect Tile Tile deriving Ord

instance Eq Connected where
         (Connect a b) == (Connect a' b') = (a == a' && b == b') ||
                                            (a == b' && b == a')

linked :: Connected -> Connected -> Bool
linked (Connect a b) (Connect a' b') = (a == a' || b == b' || a == b' || b == a')

links :: Connected -> Tile -> Maybe Tile
links (Connect a b) t | (a == t) = Just b
                      | (b == t) = Just a
                      | otherwise = Nothing



-- Applies a function to both elements of a tuple

onPair :: (a -> b) -> (a, a) -> (b, b)
onPair f (x, y) = (f x, f y)
