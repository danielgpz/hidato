import qualified System.Environment as SE
import qualified Control.Monad as CM
import qualified Data.Time as DT
import qualified Game.Hidato as GH

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
        let hidato = (read lines) :: GH.Hidato

        putStrLn $ "Solving Hidato...\n"
        stime <- DT.getCurrentTime
        
        -- let sols = [head (GH.solveHidato hidato)]
        let sols = (GH.solveHidato hidato)
        CM.mapM print sols
        
        
        etime <- DT.getCurrentTime
        let dtime = (DT.utctDayTime etime) - (DT.utctDayTime stime)
        putStrLn $ "Found " ++ show (length sols) ++ " solution(s) in " ++ show dtime
        
        getLine