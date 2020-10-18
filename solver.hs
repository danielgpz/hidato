import qualified Control.Monad as CM
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


main = 
    do
        lines <- getLines2
        let hidato = (read lines) :: GH.Hidato

        putStrLn $ "Solving Hidato...\n"
        let sols = GH.solveHidato hidato
        
        putStrLn $ "Found " ++ show (length sols) ++ " solution(s):\n"
        CM.mapM print sols
        
        getLine