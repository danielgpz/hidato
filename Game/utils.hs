module Game.Utils(
    stringToTable, tableToString, 
    Cell, InfoCell, MarkedCell, cellDistance, 
    Direction, moveInDirection, getNeighbours,
    genUniform, genDirections) where

import qualified Data.List as DL
import qualified System.Random as RD

stringToTable :: String -> [[String]]
stringToTable = map DL.words . DL.lines

tableToString :: [[String]] -> String
tableToString [] = ""
tableToString table = 
    let
        max_lengths = map (maximum . map length) table
        max_length = maximum max_lengths
        new_table = map (map (tab max_length)) table
    in 
        DL.unlines $ map DL.unwords new_table
    where
        tab :: Int -> String -> String
        tab n s = (replicate (n - (length s)) ' ') ++ s

--A cell, just a pair of Int
type Cell = (Int, Int)
type InfoCell = (Maybe Int, Cell)
type MarkedCell = (Int, Cell)

cellDistance :: Cell -> Cell -> Int
cellDistance (x, y) (v, w) = max (abs(x - v)) (abs(y - w))

--8-directions
data Direction = N | NE | E | SE | S | SW | W | NW deriving(Enum)

moveInDirection :: Cell -> Direction -> Cell
moveInDirection (x, y) N  = (x - 1, y)
moveInDirection (x, y) S  = (x + 1, y)
moveInDirection (x, y) E  = (x, y + 1)
moveInDirection (x, y) W  = (x, y - 1)
moveInDirection (x, y) NE = (x - 1, y + 1)
moveInDirection (x, y) SW = (x + 1, y - 1)
moveInDirection (x, y) NW = (x - 1, y - 1)
moveInDirection (x, y) SE = (x + 1, y + 1)

getNeighbours :: Direction -> Cell -> [Cell]
getNeighbours dir c = 
    let 
        i = fromEnum dir
        dirs = [toEnum(if j >= 8 then j - 8 else j) | j <- [i .. (i + 7)]]
    in map (moveInDirection c) dirs

genUniform' :: RD.StdGen -> Int -> [Int] 
genUniform' gen k = 
    let (n, ngen) = RD.random gen 
    in (mod n k:genUniform' ngen k)

genUniform :: Int -> Int -> [Int]
genUniform seed = genUniform' (RD.mkStdGen seed)

genDirections :: Int -> [Direction]
genDirections seed = map toEnum (genUniform seed 8)


