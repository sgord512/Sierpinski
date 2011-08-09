--Filename: sierpinski.hs
--Project: Sierpinski, just trying to make Sierpinski's Triangle in Haskell. Testing the Gloss library. (It is quite nice.)
--Author: Spencer Gordon
--Date: August 7th, 2011

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate
import System.Environment

windowTitle = "Sierpinski (by Spencer Gordon)"

{-- Where to position the window on the screen --}

offsetFromOrigin :: (Int, Int)
offsetFromOrigin = (fromInteger 200, fromInteger 200)

{-- Size of window --}

windowDim :: Int
windowDim = 600

{-- A convenient tuple holding the vertices of a triangle --}

type Triangle = (Point, Point, Point)

{-- The outermost triangle. Gloss uses a coordinate system so that the origin is in the center of the window --}

baseTriangle :: Triangle
baseTriangle = let d = fromIntegral windowDim
                   l = (3 * d / 4)
                   h = (sqrt 3) * l / 2
               in ( ((-l) / 2, (-h) / 2) -- I can transform the triangle by changing any of these coordinates, and that transformation will be reflected in the final shape
                  , (0, h / 2)
                  , (l / 2, (-h) / 2)
                  )

main = do input <- getArgs
          case input of
               [] -> simulateInWindow windowTitle
                                      (windowDim, windowDim)
                                      offsetFromOrigin
                                      black
                                      1
                                      sierpinski
                                      (\(t:ts) -> Pictures (map (Color green . toPicture) t))
                                      (\view float (t:ts) -> ts)
               x:_ -> do depth <- readIO x
                         displayInWindow windowTitle
                                         (windowDim, windowDim) 
                                         offsetFromOrigin
                                         black
                                         (stillImage depth)

{-- Takes an int and constructs a triangle with that many layers of recursion (1 corresponds to a single triangle) --}
                         
sierpinski :: [[Triangle]]
sierpinski = (iterate (concatMap (fst . recur)) [baseTriangle])

stillImage :: Int -> Picture
stillImage x = Pictures (map (Color green . toPicture) (sierpinski !! x))

{-- Takes a triangle and returns the four triangles that it can be split into (I return the center triangle separately, so that it can be handled separately, perhaps with different coloring --}

recur :: Triangle -> ([Triangle], Triangle)
recur t@(a, b, c) = let (ab, bc, ca) = midpoints t
                    in ([(a, ab, ca)
                        ,(b, bc, ab)
                        ,(c, bc, ca)
                        ]
                        ,(ab, bc, ca))

{-- Converts from my Triangle to a Picture, by making a corresponding Polygon --}

toPicture :: Triangle -> Picture
toPicture (a, b, c) = Polygon [a, b, c] 

{-- The midpoints of the three edges of a triangle, ordered according to the order of the vertices in the provided triangle --}

midpoints :: Triangle -> Triangle
midpoints (a, b, c) = (midpoint a b, midpoint b c, midpoint c a)

{-- The midpoint formula --}

midpoint :: Point -> Point -> Point
midpoint (a, b) (a', b') = ((a + a') / 2, (b + b') /2)

