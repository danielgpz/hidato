module Game.Hidato(Hidato, fromList, solveHidato) where
import qualified Data.IntMap as IM
import qualified Data.Set as DS
import qualified Data.List as DL
import Game.Utils(stringToTable, tableToString, Cell, InfoCell, MarkedCell, cellDistance, Direction(..), getNeighbours, genDirections)

--A Hidato table, ocupied cells with numbers, free cells to be set, and the starting and ending numbers. 
data Hidato = Hid { mcells :: (IM.IntMap Cell), ucells :: (DS.Set Cell), start :: MarkedCell, end :: MarkedCell }

--Creates a Hidato from a List with cell's cordenates and his possible numbers
fromList :: [(InfoCell)] -> Hidato
fromList cells =
    let
        mcells = IM.fromList [(v, c) | (Just v, c) <- cells]
        ucells = DS.fromList [c | (Nothing, c) <- cells]
        Just start = IM.lookupMin mcells
        Just end   = IM.lookupMax mcells
    in Hid mcells ucells start end

-- Just move on to the next cell and mark it, return a new hidato if move was legal
markCell :: Cell -> Hidato -> Maybe Cell -> Maybe Hidato
markCell cell (Hid mcells ucells start end) mc = case mc of
    Just c -> 
        if c == cell then Just $ Hid mcells ucells (next, cell) end
        else Nothing
    Nothing -> 
        if DS.member cell ucells then Just $ Hid (IM.insert next cell mcells) (DS.delete cell ucells) (next, cell) end
        else Nothing
    where
        next = succ . fst $ start

-- markCell :: Cell -> Hidato -> MarkedCell -> Maybe Hidato
-- markCell cell (Hid mcells ucells start end) (p, c)
--     | cell == c && next == p = Just $ Hid mcells ucells (next, cell) end
--     | dist > (p - next)      = Nothing
--     | DS.member cell ucells  = Just $ Hid (IM.insert next cell mcells) (DS.delete cell ucells) (next, cell) end
--     | otherwise              = Nothing
--     where
--         next = succ . fst $ start
--         dist = cellDistance cell c

--Solver function, a trivial backtrack thats travel all posibles paths
solveHidato :: Hidato -> [Hidato]
solveHidato h 
    | start h == end h = [h]
solveHidato h = concatMap backtrack . getNeighbours N $ cell
    where
        (pos, cell)  = start $ h
        mc = IM.lookup (succ pos) (mcells h)
        -- Just mc = IM.lookupGE (succ pos) (mcells h)
        backtrack c = case markCell c h mc of
            Just h' -> solveHidato h'
            Nothing -> []

--Making Hidato friend of class Read for set custom read
instance Read Hidato where
    readsPrec _ s = 
        let 
            table = stringToTable s
            readrow row i = [(readCell c, (i, j)) | (c, j) <- zip row [1..], c /= "-"]
            cells = concat (zipWith readrow table [1..])
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
            tableToString $ map (fill_from 1) table
        where
            fill_from _ [] = []
            fill_from i l@((c, p) : xs)
                | i < snd c     = "-" : fill_from (i + 1) l
                | otherwise = (putCell p) : fill_from (i + 1) xs

            putCell Nothing = "+"
            putCell (Just p) = show p

