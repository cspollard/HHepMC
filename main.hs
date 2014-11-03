module Main where

import Data.HepMC.Parser.Common
import Data.HepMC.HepMCFile
import Data.Queue
import qualified Data.Text.Lazy.IO as TIO
import System.Environment (getArgs)


main :: IO ()
main = do
    text <- TIO.readFile =<< fmap head getArgs

    -- let r = parse (skipSpace *> parseVersion *> parseBeginEventsLine *> parseEvent) text

    -- case r of
        -- Done t _ -> print $ fmap head (parse (many' parseEvent) t)
        -- _ -> print "error"

    -- let o = fmap (take 0 . events) $ maybeResult (parse parserHepMC text)

    let line = takeTill isEndOfLine <* endOfLine
    let r = maybeResult $ parse (manyQ line) text

    print $ takeQ 5 <$> r
