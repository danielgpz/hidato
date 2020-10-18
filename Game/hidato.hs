module Game.Hidato(Hidato, fromList, solveHidato) where
import qualified Data.List as DL
import qualified Data.IntMap as IM
import qualified Data.Set as DS
import qualified Game.Utils as U

--A cell, just a pair of Int
type Cell = (Int, Int)
type InfoCell = (Cell, Maybe Int)
type MarkedCell = (Int, Cell)

--A Hidato table, ocupied cells with numbers, free cells to be set, and the starting and ending numbers. 
data Hidato = Hid { mcells :: (IM.IntMap Cell), ucells :: (DS.Set Cell), start :: MarkedCell, end :: MarkedCell }

--Creates a Hidato from a List with cell's cordenates and his possible numbers
fromList :: [(InfoCell)] -> Hidato
fromList cells =
    let
        mcells = [(v, c) | (c, Just v) <- cells]
        ucells = [c | (c, Nothing) <- cells]
        start  = minimum mcells
        end    = maximum mcells
    in Hid (IM.fromList mcells) (DS.fromList ucells) start end

directions :: [Cell]
directions = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

getNeighbours :: Cell -> [Cell]
getNeighbours (x, y) = map (\(v, w) -> (x + v, y + w)) directions

--Just move on to the next cell and mark it, return a new hidato if move was legal
markCell :: Cell -> Hidato -> Maybe Hidato
markCell cell (Hid mcells ucells start end)
    | IM.lookup next mcells == Just cell  = Just $ Hid mcells ucells (next, cell) end
    | DS.member cell ucells               = Just $ Hid (IM.insert next cell mcells) (DS.delete cell ucells) (next, cell) end
    | otherwise                           = Nothing
    where
        next = succ . fst $ start

--Solver function, a trivial backtrack thats travel all posibles paths
solveHidato :: Hidato -> [Hidato]
solveHidato h
    | sc == ec = [h]
    | s == e   = []
    where
        (s, sc) = start h
        (e, ec) = end h
solveHidato h = backtrack . getNeighbours . snd . start $ h  
    where
        backtrack [] = []
        backtrack (c:cs) =
            case markCell c h of
                Just h  -> solveHidato h ++ backtrack cs
                Nothing -> backtrack cs

--Making Hidato friend of class Read for set custom read
instance Read Hidato where
    readsPrec _ s = 
        let 
            table = U.stringToTable s
            readrow row i = [((i, j), readCell c) | (c, j) <- zip row [1..], c /= "-"]
            cells = foldl1 (++) (zipWith readrow table [1..])
        in
            [(fromList cells, "")]
        where
            readCell :: String -> Maybe Int
            readCell "+" = Nothing
            readCell v   = Just (read v)

--Making Hidato friend of class Show for set custom show
instance Show Hidato where
    show (Hid mcells ucells start end) = 
        let
            mcells_list = map (\(p, c) -> (c, Just p)) (IM.toList mcells)
            ucells_list = map (\c -> (c, Nothing)) (DS.toList ucells)
            all_cells = DL.sort $ mcells_list ++ ucells_list
            table = DL.groupBy (\(a,_) (b,_) -> fst a == fst b) all_cells
        in 
            U.tableToString $ map (fill_from 1) table
        where
            fill_from _ [] = []
            fill_from i l@((p, c) : xs)
                | i < snd p     = "-" : fill_from (i + 1) l
                | otherwise = (putCell c) : fill_from (i + 1) xs

            putCell Nothing = "+"
            putCell (Just p) = show p

