module Main where

import Data.HepMC.Parser
import qualified Data.Attoparsec.Text.Lazy as APT
import qualified Data.Text.Lazy.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
    text <- TIO.readFile =<< head `fmap` getArgs

    APT.parseTest hepMCParser text
