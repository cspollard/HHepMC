module Main where

import Data.HepMC.Parser
import Data.HepMC.HepMCFile
import Data.HepMC.Event
import Data.HepMC.Particle
import qualified Data.Attoparsec.Text.Lazy as APT
import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy.IO as TIO
import System.Environment (getArgs)
import System.IO (getContents)
import Control.Monad (mapM_)
import Data.Maybe (fromJust)
import qualified Data.IntMap as IM

main :: IO ()
main = do
    fname <- head `fmap` getArgs
    text <- case fname of
        "-" -> pack `fmap` getContents
        _ -> TIO.readFile fname

    let f = fromJust . APT.maybeResult . APT.parse hepMCFileParser $ text

    let (fsParts, isParts) = IM.partition ((== 2)  . partStatus) $ (eventParticles . head . events ) f

    mapM_ print $ IM.elems fsParts
