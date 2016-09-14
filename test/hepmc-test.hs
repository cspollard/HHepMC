module Main where

import Conduit
import Data.Conduit.Attoparsec

import Data.HepMC.Parse
import Data.HepMC.Event
import System.Environment (getArgs)




main :: IO ()
main = do
    f <- head <$> getArgs
    runResourceT $
            sourceFile f
            =$= (sinkParser parserVersion
                    >> conduitParser parserEvent)
            $$  mapM_C (liftIO . print)


-- printEvent :: Event -> IO ()
-- printEvent = print . length . S.filter ((&&) <$> hasPID 2212 <*> final) . view egParts . graph
-- printEvent = print . map partPID . filter (\n' -> or . map (\p -> let pID = pid p in hasBottomQuark pID && hadron pID) . ancestors $ n') . egFinalParts . evtGraph
