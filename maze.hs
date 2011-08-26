--Filename: maze.hs
--Project: A maze generator and solver
--Author: Spencer Gordon
--Date: August 11th, 2011

module Main where

import Base.Graphics
import Base.Graphics.Djikstra
import Base.Graphics.Partition
import Base.Grid
import Base.Maze
import Base.Path
import Base.Toolbox ( onPair )
import Control.Monad ( foldM )
import Control.Monad.State
import Data.Array ( array, elems )
import Data.List ( (\\) )
import Data.Maybe ( fromJust, isJust )
import qualified Data.Set as Set
import Debug.Trace
import Graphics.Gloss hiding ( dim )
import System
import System.Console.GetOpt
import System.Random

main = do input <- getArgs
          let (opts, nonOpts, err) = getOpt Permute optionsList input
              options = foldr ($) defaultOptions opts 
          gen <- if isJust (seed options) then return $ fromJust (seed options) else getStdGen
          let startingBoard = (setUpMaze (dim options) gen)              
              (pair, boardWithStartAndEnd) = runState (pickStartAndEnd gen) startingBoard
              startingDjikstra = setUpDjikstra pair boardWithStartAndEnd
              (_, searchStateList) = generateDjikstraList startingDjikstra
              imagesList = map (djikstraStatePicture startingDjikstra) searchStateList
          animatedDisplay options imagesList      
--          (dkResult, finalBoard) = runState searchForPath startingDjikstra
--              (tileGroups, board) = runState partitionBoard startingBoard
--              images = boardToDjikstraPicture dkResult (board finalBoard)
--              images = boardToPlainPicture startingBoard
--          staticDisplay options images
          

-- Calculates the size of a window from its windows and columns

sizeFromRowsCols :: (Int, Int) -> (Int, Int)
sizeFromRowsCols rc = ((ceiling .  (\s -> (s + 2) * (gap + tileRadius) + 2 * gap) . fromIntegral) `onPair` rc)

-- Takes a single picture and the options and displays that

staticDisplay :: Options -> Picture -> IO ()
staticDisplay options images = displayInWindow "Maze Generator by Spencer Gordon" (sizeFromRowsCols $ dim options) corner black images

-- This will restart the animation from the beginning when it reaches the end of the list

animatedDisplay :: Options -> [Picture] -> IO ()
animatedDisplay options pics = animateInWindow "Maze Generator by Spencer Gordon" (sizeFromRowsCols $ dim options) corner black (\time -> pics !! ((truncate (12 * time)) `mod` (length pics)))

-- All the stuff I need in place to handle the option processing

data Options = Options { dim :: (Int, Int)
                       , seed :: (Maybe StdGen)
                       }

defaultOptions :: Options
defaultOptions = Options { dim = (21, 21)
                         , seed = Nothing
                         }

optionsList :: [ OptDescr (Options -> Options) ]
optionsList = [ Option ['s'] ["size"] (ReqArg (\arg opt -> opt { dim = let n = read arg in (n, n) })  "SIDE LENGTH OF SQUARE") "Choose how large a maze you would like to create" 
              , Option ['g'] ["generator"] (ReqArg (\arg opt -> opt { seed = Just (mkStdGen $ read arg) }) "INT USED TO GENERATE MAZE") "Specify a seed with which to generate your board for deterministic results"
              ]
