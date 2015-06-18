module Main where

import Data.HepMC.Parse
import Data.HepMC.File hiding (events)
import Data.HepMC.Event
import Data.HepMC.Vertex
import Data.HepMC.PID
import Data.HepMC.FourMomentum
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TIO (readFile)
import System.Environment (getArgs)


main :: IO ()
main = do
    text <- TIO.readFile =<< fmap head getArgs

    let r = parse (skipSpace *> parserVersion) text

    case r of
        Done t _ -> printAllEvents t
        _ -> print "error"


printEvent :: Event -> IO ()
printEvent = print . fmap (vtxID . partParentVtx) . filter (\p -> ptV p > 100 && elem (pid p) [-16, -14, -12, 12, 14, 16]) . egParts . evtGraph


-- loop over and print all events
printAllEvents :: Text -> IO ()
printAllEvents t =
    case parse parserEvent t of
        Done r evt -> printEvent evt >> printAllEvents r
        _ -> return ()
