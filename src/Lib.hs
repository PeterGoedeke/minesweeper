module Lib where

import qualified Data.Map.Strict as Map
import Control.Monad

type Point = (Int, Int)
type Board = Map.Map Point Tile

data Mine = Mine | Clear deriving (Eq, Show)

data State = Flagged | Known | Unknown deriving (Eq, Show)

data Tile = Tile {
    isMine :: Bool,
    status :: State
}

-- functions for finding various categories of tiles adjacent to a given tile of the board

getAdjacents :: Point -> Board -> [Tile]
getAdjacents (x, y) board = [x | Just x <- adjacents]
    where
        coords = [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], not (x == i && y == j)]
        adjacents = fmap (flip Map.lookup board) coords 

getAdjacentsOf :: State -> Point -> Board -> [Tile]
getAdjacentsOf t p board = filter (\x -> status x == t) $ getAdjacents p board

getAdjacentFlags :: Point -> Board -> [Tile]
getAdjacentFlags = getAdjacentsOf Flagged

getAdjacentUnknowns :: Point -> Board -> [Tile]
getAdjacentUnknowns = getAdjacentsOf Unknown

getAdjacentMines :: Point -> Board -> [Tile]
getAdjacentMines p board = filter isMine $ getAdjacents p board