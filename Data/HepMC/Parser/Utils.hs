module Data.HepMC.Parser.Utils where

import Data.Attoparsec.Text.Lazy

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL


parseWithSpace :: Parser a -> Parser a
parseWithSpace p = do
    x <- p; skipSpace

    return x

dec :: Parser Int
dec = parseWithSpace (signed decimal)

doub :: Parser Double
doub = parseWithSpace (signed double)


parseList :: Int -> Parser a -> Parser [a]
parseList n p = do
    count n (parseWithSpace p)


parseQuote :: Parser TL.Text
parseQuote = do
    skipSpace
    char '"'
    x <- takeTill (== '"')
    char '"'
    return $ TL.fromStrict x


toEndLine :: Parser TL.Text
toEndLine = do
    s <- takeTill isEndOfLine
    endOfLine
    return $ TL.fromStrict s
