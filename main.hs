module Main where

import Data.HepMC.HepMCFile
import qualified Data.Attoparsec.Text.Lazy as APT
import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy.IO as TIO
import System.Environment (getArgs)
import System.IO (getContents)
import Control.Monad (mapM_)
import Data.Maybe (fromJust)
import qualified Data.IntMap as IM

import Control.Applicative

main :: IO ()
main = do
    text <- TIO.readFile =<< fmap head getArgs

    let r = APT.parse (APT.skipSpace *> parseVersion *> parseBeginEventsLine *> parseEvent) text

    case r of
        APT.Done t _ -> print $ fmap head (APT.parse (APT.many' parseEvent) t)
        _ -> print "error"

    -- let o = fmap (take 0 . events) $ (APT.eitherResult . APT.parse hepMCFileParser) text

    -- print o
