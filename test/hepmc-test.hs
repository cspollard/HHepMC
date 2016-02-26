module Main where

import Data.HepMC.Parse
import Data.HepMC.Event
import Data.HepMC.EventGraph
import Data.HEP.PID
import Data.HepMC.Barcoded
import Data.HepMC.Vertex
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Set as S
import System.Environment (getArgs)
import Control.Arrow
import Debug.Trace




main :: IO ()
main = do
    r <- parse (skipSpace *> parserVersion) <$> BS.getContents

    case r of
        Done t s -> print s >> printAllEvents t 0
        _ -> error "error"


printEvent :: Event -> IO ()
printEvent = print . length . S.filter (isPID 2212) . S.filter final . egParts . graph
-- printEvent = print . map partPID . filter (\n' -> or . map (\p -> let pID = pid p in hasBottomQuark pID && hadron pID) . ancestors $ n') . egFinalParts . evtGraph


-- loop over and print all events
printAllEvents :: ByteString -> Int -> IO ()
printAllEvents t n =
    case parse (eitherP parserEvent $ return ()) t of
        Done r (Left evt) -> printEvent evt >> printAllEvents r (n+1)
        Done r (Right _) -> print $ "nevts: " ++ show n
        Fail bs conts err -> print conts >> print err
        Partial _ -> print "didn't finish???"
