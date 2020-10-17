module Game.Hidato(Hidato, fromList) where
import qualified Data.List as DL
import qualified Data.IntMap as IM
import qualified Data.Set as DS
import qualified Game.Utils as U

--A cell, just a pair of Int
type Cell = (Int, Int)

--A Hidato table, ocupied cells with numbers, free cells to be set, and the starting and ending numbers. 
data Hidato = Hid { mcells :: (IM.IntMap Cell), ucells :: (DS.Set Cell), start :: Int, end :: Int }

fromList :: [(Int, Cell)] -> Hidato
fromList cells =
    let
        (mcells, ucells) = DL.partition ((/=0) . fst) cells
        mcells_map = IM.fromList mcells 
        ucells_set = DS.fromList . map snd $ ucells
        start  = fst . minimum $ mcells
        end    = fst . maximum $ mcells
    in Hid mcells_map ucells_set start end

instance Read Hidato where
    readsPrec _ s = 
        let 
            table = U.stringToTable s
            readrow row i = [(readCell c, (i, j)) | (c, j) <- zip row [1..], c /= "-"]
            cells = foldl1 (++) (zipWith readrow table [1..])
        in
            [(fromList cells, "")]
        where
            readCell :: String -> Int
            readCell "+" = 0
            readCell v = read v

instance Show Hidato where
    show (Hid mcells ucells start end) = 
        let
            mcells_list = map (\(p, c) -> (c, p)) (IM.toList mcells)
            ucells_list = map (\c -> (c, 0)) (DS.toList ucells)
            all_cells = DL.sort $ mcells_list ++ ucells_list
            table = DL.groupBy (\((x1, _), _) ((x2, _), _) -> x1 == x2) all_cells
        in 
            U.tableToString $ map (fill_row 1) table
        where
            fill_row _ [] = []
            fill_row p l@(((x, y), c) : xs)
                | p < y     = "-" : fill_row (p + 1) l
                | otherwise = (putCell c) : fill_row (p + 1) xs

            putCell n
                | n == 0     = "+"
                | n == start = "(" ++ show n ++ ")"
                | n == end   = "[" ++ show n ++ "]"
                | otherwise  = show n

