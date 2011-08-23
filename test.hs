module Main where

import Test.QuickCheck
import Base.Graphics
import Base.Grid
import Base.Maze
import Base.Path
       
main =  do mapM_ verboseCheck properties
           putStrLn "Hello, World."

dim = (21, 21)

properties = [\tp -> (pathLength tp) == (length pathToGroup)]

instance Arbitrary TilePath where
         arbtrary = Path choose (