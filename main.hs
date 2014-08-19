module Main where

import qualified HEP.Parser.HepMC.Parser as HMC
import qualified Data.Attoparsec.Text.Lazy as APT
import qualified Data.Text.Lazy.IO as TIO
import System.Environment (getArgs)


-- parseHMC :: Text -> 

main :: IO ()
main = do
    args <- getArgs
    let fname = args !! 0
    text <- TIO.readFile fname

    let header = APT.parse HMC.hepmcHeader text

    print header

    return ()
