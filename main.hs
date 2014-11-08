module Main where

import Data.HepMC.Parser.Common
import Data.HepMC.HepMCFile hiding (events)
import Data.HepMC.Event
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TIO (readFile)
import System.Environment (getArgs)


main :: IO ()
main = do
    text <- TIO.readFile =<< fmap head getArgs

    r <- return $ parse (skipSpace *> parserVersion) text

    case r of
        Done t _ -> events t
        _ -> print "error"


-- this is the answer.
events :: Text -> IO ()
events t = do
    case parse parserEvent t of
        Done t' evt -> do
            print evt
            events t'
        _ -> return ()
