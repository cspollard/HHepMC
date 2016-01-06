module Main where

import Data.HepMC.Parse
import Data.HepMC.File hiding (events)
import Data.HepMC.Event
import Data.HepMC.EventGraph
import Data.HepMC.PID
import Data.HepMC.Barcoded
import Data.HepMC.Vertex
import Data.HepMC.FourMomentum
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TIO (readFile)
import System.Environment (getArgs)
import Control.Arrow


main :: IO ()
main = do
    text <- TIO.readFile =<< fmap head getArgs

    let r = parse (skipSpace *> parserVersion) text

    case r of
        Done t _ -> printAllEvents t
        _ -> error "error"


printEvent :: Event -> IO ()
printEvent = print . map (\p -> (bc p, pid p)) . filter isPrompt . egParts . evtGraph
-- printEvent = print . map partPID . filter (\n' -> or . map (\p -> let pID = pid p in hasBottomQuark pID && hadron pID) . ancestors $ n') . egFinalParts . evtGraph


-- loop over and print all events
printAllEvents :: Text -> IO ()
printAllEvents t =
    case parse parserEvent t of
        Done r evt -> printEvent evt >> printAllEvents r
        _ -> return ()
