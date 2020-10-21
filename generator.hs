import qualified System.Environment as SE
import System.Random(getStdGen, newStdGen)
import qualified Control.Monad as CM
import qualified Data.Time as DT
import Game.Hidato(Hidato, fromList)
import Game.Utils(genDirections, Shape(Rectangle, Pyramid, Stairs), genShape)

main = 
    do
        gen <- getStdGen
        print $ take 30 (genDirections gen)

        gen1 <- newStdGen
        let rhidato = fromList . genShape gen1 $ Rectangle 7 8
        print $ rhidato

        gen2 <- newStdGen
        let phidato = fromList . genShape gen2 $ Pyramid 6
        print $ phidato

        gen3 <- newStdGen
        let shidato = fromList . genShape gen3 $ Stairs 8
        print $ shidato

        -- getLine