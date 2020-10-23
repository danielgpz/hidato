import System.Environment(getArgs)
import System.Random(getStdGen, newStdGen)
import System.Timeout(timeout)
import Data.Time(getCurrentTime, utctDayTime)
import Data.List(unwords)
import Game.Table(Shape)
import Game.Hidato(Hidato, genHidato)

getInput :: IO String
getInput =
    do
        args <- getArgs
        case args of
            [] -> getLine
            _  -> return (unwords args)

generator :: Shape -> IO ()
generator shape = 
    do
        putStrLn $ "Generating " ++ show shape ++ " ...\n"
        gen <- getStdGen
        result <- timeout 5000000 $ print (genHidato gen shape)
        case result of
            Nothing -> do
                putStrLn $ "Time out!, trying again...\n"
                newStdGen 
                generator shape       
            Just () -> return ()
 
main = 
    do
        input <- getInput
        let shape = read input :: Shape
        
        stime <- getCurrentTime
        generator shape
        etime <- getCurrentTime
        
        let dtime = (utctDayTime etime) - (utctDayTime stime)
        putStrLn $ "Generated in " ++ show dtime

        getLine