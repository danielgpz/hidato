module Game.Table(stringToTable, tableToString, Cell, InfoCell, MarkedCell, cellDistance, 
    Direction(..), moveOnDirection, getNeighbours, genDirections, Shape(..), genShape) where

import Data.List(words, unwords, lines, unlines, find)
import Data.Set(Set, member, fromList)
import System.Random(Random, random, randomR, randomRs, StdGen)

--A cell, just a pair of Int
type Cell = (Int, Int)
type InfoCell = (Maybe Int, Cell)
type MarkedCell = (Int, Cell)

cellDistance :: Cell -> Cell -> Int
cellDistance (x, y) (v, w) = max (abs(x - v)) (abs(y - w))

--8-directions type
data Direction = N | S | E | W | NE | SW | SE | NW deriving(Show, Read, Enum, Bounded)

--Making it randomizable
instance Random Direction where
    random gen =
        let 
            (r, ngen) = random gen
            dir = toEnum (mod r 8)
        in (dir, ngen)

    randomR (d1, d2) gen =
        let 
            (r, ngen) = randomR (fromEnum d1, fromEnum d2) gen
            dir = toEnum (mod r 8)
        in (dir, ngen)

moveOnDirection :: Cell -> Direction -> Cell
moveOnDirection (x, y) N  = (x - 1, y)
moveOnDirection (x, y) S  = (x + 1, y)
moveOnDirection (x, y) E  = (x, y + 1)
moveOnDirection (x, y) W  = (x, y - 1)
moveOnDirection (x, y) NE = (x - 1, y + 1)
moveOnDirection (x, y) SW = (x + 1, y - 1)
moveOnDirection (x, y) NW = (x - 1, y - 1)
moveOnDirection (x, y) SE = (x + 1, y + 1)

getNeighbours :: Direction -> Cell -> [Cell]
getNeighbours dir c = 
    let 
        i = fromEnum dir
        dirs = [toEnum(if j >= 8 then j - 8 else j) | j <- [i .. (i + 7)]]
    in map (moveOnDirection c) dirs

genDirections :: StdGen -> [Direction]
genDirections = randomRs (N, NW)


--Shape type, for store a generic shape of a table
data Shape = Rectangle Int Int | Pyramid Int | Stairs Int deriving(Show, Read)

--Generates all cells of a generic shape
createShape :: Shape -> [Cell]
createShape (Rectangle n m) = [(i, if odd i then j else m + 1 - j) | i <- [1..n], j <- [1..m]]
createShape (Pyramid n) = concat [[(i, if odd i then j else 2 * n - j) | j <- take (2 * i - 1) [(n + 1 - i)..]] | i <- [1..n]]
createShape (Stairs n) = concat [[(i, if odd i then j else i + 1 - j) | j <- [1..i]] | i <- [1..n]]

backBite :: (Eq a) => [a] -> a -> [a]
backBite l x = 
    let (left, right) = break (==x) l
    in reverse left ++ right

--For randomize once the path traveling the table
randomizePath :: Set Cell -> [Cell] -> Direction -> [Cell]
randomizePath set cells@(c1:c2:cs) dir =
    let 
        neighbours = getNeighbours dir c1
        fn = find (flip member set) neighbours
    in case fn of 
        Just c -> backBite cells c
        Nothing -> cells
randomizePath _ cells _ = cells

--Starting with a trivial path, randomize the path followings that directions
randomizeTable :: [Cell] -> [Direction] -> [Cell]
randomizeTable cells = foldl (randomizePath (fromList cells)) cells

--Creates the trivial path, and randomize it using k(based on shape's dimensions) steps. Finally enums the path
genShape :: StdGen -> Shape -> [InfoCell]
genShape gen shape = 
    let rtable = randomizeTable (createShape shape) $ take k (genDirections gen)
    in zipWith (\c p -> (Just p, c)) rtable [1..]
    where
        k = case shape of 
            Rectangle n m -> (n * m) ^ 2
            Pyramid n     -> (n * n) ^ 2
            Stairs n      -> (div (n * n + n) 2) ^ 2
            _             -> 0

--Funtions to read and show formatted tables
stringToTable :: String -> [[String]]
stringToTable = map words . lines

tableToString :: [[String]] -> String
tableToString [] = ""
tableToString table = 
    let
        max_lengths = map (maximum . map length) table
        max_length = maximum max_lengths
        new_table = map (map (tab max_length)) table
    in 
        unlines $ map unwords new_table
    where
        tab :: Int -> String -> String
        tab n s = (replicate (n - (length s)) ' ') ++ s