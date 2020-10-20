import qualified System.Environment as SE
import qualified Control.Monad as CM
import qualified Data.Time as DT
import Game.Hidato(Hidato, solveHidato)

getLines :: IO [String]
getLines =
    do
        line <- getLine
        if null line then
            return []
        else do
            lines <- getLines
            return (line:lines)

getLines2 :: IO String
getLines2 =
    do
        line <- getLine
        if null line then
            return []
        else do
            lines <- getLines2
            return (line ++ '\n':lines)

getInput :: IO String
getInput =
    do
        args <- SE.getArgs
        case args of
            []      -> getLines2
            (arg:_) -> readFile arg


main = 
    do
        lines <- getInput
        let hidato = (read lines) :: Hidato

        putStrLn $ "Solving Hidato...\n"
        stime <- DT.getCurrentTime
        
        -- let sols = [head (solveHidato hidato)]
        let sols = solveHidato hidato
        CM.mapM print sols
        
        
        etime <- DT.getCurrentTime
        let dtime = (DT.utctDayTime etime) - (DT.utctDayTime stime)
        putStrLn $ "Found " ++ show (length sols) ++ " solution(s) in " ++ show dtime
        
        getLine