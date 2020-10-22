import System.Environment(getArgs)
import System.Random(getStdGen, newStdGen)
import Data.Time(getCurrentTime, utctDayTime)
import Data.List(unwords)
import Game.Hidato(genHidato)

getInput :: IO String
getInput =
    do
        args <- getArgs
        case args of
            [] -> getLine
            _  -> return (unwords args)

main = 
    do
        input <- getInput
        gen <- getStdGen
        
        stime <- getCurrentTime
        let hidato = genHidato gen $ read input
        print $ hidato
        etime <- getCurrentTime
        
        -- let dtime = (utctDayTime etime) - (utctDayTime stime)
        -- putStrLn $ "Generated in " ++ show dtime

        -- getLine