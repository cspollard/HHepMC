module Main where

import Data.HepMC.Parse
import Data.HepMC.HepMCFile hiding (events)
import Data.HepMC.Event
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TIO (readFile)
import System.Environment (getArgs)


main :: IO ()
main = do
    text <- TIO.readFile =<< fmap head getArgs

    let r = parse (skipSpace *> parserVersion) text

    case r of
        Done t _ -> events t
        _ -> print "error"


-- loop over and print all events
events :: Text -> IO ()
events t =
    case parse parserEvent t of
        Done t' evt -> do
            print evt
            events t'
        _ -> return ()
