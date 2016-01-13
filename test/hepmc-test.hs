module Main where

import Data.HepMC.Parse
import Data.HepMC.File hiding (events)
import Data.HepMC.Event
import Data.HepMC.EventGraph
import Data.HEP.PID
import Data.HepMC.Barcoded
import Data.HepMC.Vertex
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TIO
import System.Environment (getArgs)
import Control.Arrow
import Debug.Trace


main :: IO ()
main = do
    r <- parse (skipSpace *> parserVersion) <$> TIO.getContents

    case r of
        Done t s -> print s >> printAllEvents t 0
        _ -> error "error"


printEvent :: Event -> IO ()
printEvent = print . map (bc &&& pid) . egParts . graph
-- printEvent = print . map partPID . filter (\n' -> or . map (\p -> let pID = pid p in hasBottomQuark pID && hadron pID) . ancestors $ n') . egFinalParts . evtGraph


-- loop over and print all events
printAllEvents :: Text -> Int -> IO ()
printAllEvents t n =
    case parse (eitherP parserEvent $ return ()) t of
        Done r (Left evt) -> printEvent evt >> printAllEvents r (n+1)
        Done r (Right _) -> print $ "nevts: " ++ show n
        err -> print "error."
