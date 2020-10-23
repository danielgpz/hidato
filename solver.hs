import System.Environment(getArgs)
import System.Random(getStdGen)
import Control.Monad(mapM)
import Data.Time(getCurrentTime, utctDayTime)
import Game.Hidato(Hidato, solveHidato)

getLines :: IO String
getLines =
    do
        line <- getLine
        if null line then
            return []
        else do
            lines <- getLines
            return (line ++ '\n':lines)

getInput :: IO String
getInput =
    do
        args <- getArgs
        case args of
            []      -> getLines
            (arg:_) -> readFile arg

main = 
    do  
        input <- getInput
        let hidato = (read input) :: Hidato

        putStrLn $ "Loaded Hidato:\n\n" ++ show hidato      

        putStrLn $ "Solving Hidato...\n"
        stime <- getCurrentTime
        
        let sols = solveHidato hidato
        mapM print sols
        
        
        etime <- getCurrentTime
        let dtime = (utctDayTime etime) - (utctDayTime stime)
        putStrLn $ "Found " ++ show (length sols) ++ " solution(s) in " ++ show dtime