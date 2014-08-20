module Main where

import Data.HHepMC
import qualified Data.Attoparsec.Text.Lazy as APT
import qualified Data.Text.Lazy.IO as TIO
import System.Environment (getArgs)


-- parseHMC :: Text -> 

main :: IO ()
main = do
    args <- getArgs
    let fname = args !! 0
    text <- TIO.readFile fname

    APT.parseTest hepMCParser text
