module Game.Utils(stringToTable, tableToString) where

import qualified Data.List as DL

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
