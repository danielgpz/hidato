module Game.Utils(
    stringToTable, tableToString, 
    Cell, InfoCell, MarkedCell, cellDistance, 
    Direction(..), moveOnDirection, getNeighbours,
    genDirections, 
    Shape(Rectangle, Pyramid, Stairs, RecursiveSquare), genShape) where

import qualified Data.List as DL
import qualified Data.Set as DS
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
data Direction = N | NE | E | SE | S | SW | W | NW deriving(Show, Read, Enum, Bounded)

instance RD.Random Direction where
    random gen =
        let 
            (r, ngen) = RD.random gen
            dir = toEnum (mod r 8)
        in (dir, ngen)

    randomR (d1, d2) gen =
        let 
            (r, ngen) = RD.randomR (fromEnum d1, fromEnum d2) gen
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

genDirections :: RD.StdGen -> [Direction]
genDirections = RD.randomRs (N, NW)
-- genDirections = map toEnum . genUniform 8


data Shape = Rectangle Int Int | Pyramid Int | Stairs Int | RecursiveSquare Int deriving(Show, Read)


createShape :: Shape -> [Cell]
createShape (Rectangle n m) = [(i, if odd i then j else m + 1 - j) | i <- [1..n], j <- [1..m]]
createShape (Pyramid n) = concat [[(i, if odd i then j else 2 * n - j) | j <- take (2 * i - 1) [(n + 1 - i)..]] | i <- [1..n]]
createShape (Stairs n) = concat [[(i, if odd i then j else i + 1 - j) | j <- [1..i]] | i <- [1..n]]
createShape _ = []

backBite :: (Eq a) => [a] -> a -> [a]
backBite l x = 
    let (left, right) = break (==x) l
    in reverse left ++ right

randomizePath :: DS.Set Cell -> [Cell] -> Direction -> [Cell]
randomizePath set cells@(c1:c2:cs) dir =
    let 
        neighbours = getNeighbours dir c1
        fn = DL.find (flip DS.member set) neighbours
    in case fn of 
        Just c -> backBite cells c
        Nothing -> cells

randomizeTable :: [Cell] -> [Direction] -> [Cell]
randomizeTable cells = foldl (randomizePath (DS.fromList cells)) cells

genShape :: RD.StdGen -> Shape -> [InfoCell]
genShape gen shape = 
    let rtable = randomizeTable (createShape shape) $ take k (genDirections gen)
    in zipWith (\c p -> (Just p, c)) rtable [1..]
    where
        k = case shape of 
            Rectangle n m -> (n * m) ^ 2
            Pyramid n     -> (n * n) ^ 2
            Stairs n      -> (div (n * n + n) 2) ^ 2
            _             -> 0

