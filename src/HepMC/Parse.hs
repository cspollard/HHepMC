{-# LANGUAGE OverloadedStrings #-}

module HepMC.Parse
    ( module X
    , tuple, vector, fromFile
    ) where

import           Control.Applicative              as X ((<|>))
import           Control.Applicative              (liftA2)
import           Control.Exception.Base
import           Control.Monad                    (void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString.Char8 as X hiding (parse)
import           Data.ByteString                  (ByteString)
import qualified Data.Map.Strict                  as M
import qualified Data.Vector.Generic              as V
import           Pipes                            ((>->))
import qualified Pipes                            as P
import qualified Pipes.Attoparsec                 as PA
import qualified Pipes.ByteString                 as PB
import qualified Pipes.Parse                      as PP
import qualified Pipes.Prelude                    as P
import           System.IO


tuple :: Applicative f => f a -> f b -> f (a, b)
tuple = liftA2 (,)

vector :: V.Vector v a => Parser a -> Parser (v a)
vector p = do
  n <- decimal <* skipSpace
  V.replicateM n p

eol :: Char -> Bool
eol = isEndOfLine . toEnum . fromEnum


fromFile :: (MonadIO m, MonadThrow m) => FilePath -> m ()
fromFile fname =
  liftIO . withFile fname ReadMode $ \f -> do
    (evers, p') <- parse hmcvers $ PB.fromHandle f
    print evers

    (m, ep'') <-
      P.fold' (flip $ uncurry M.insert) mempty id
      $ PA.parsed evtHeaderLine p'

    print m

    case ep'' of
      Right _ -> throwM $ PatternMatchFail "ran out of input!"
      Left (_, p'') ->
        void . P.runEffect
        $ PA.parsed (hmcend <|> (takeTill eol <* endOfLine)) p''
          >-> P.print


hmcvers :: Parser (Int, Int, Int)
hmcvers = do
  skipSpace
  string "HepMC::Version" *> skipSpace
  x <- decimal <* char '.'
  y <- decimal <* char '.'
  z <- decimal <* skipSpace
  string "HepMC::IO_GenEvent-START_EVENT_LISTING" *> skipSpace
  return (x, y, z)


hmcend :: Parser ByteString
hmcend = do
  _ <- string "HepMC::IO_GenEvent-END_EVENT_LISTING"
  skipSpace
  return "end"


parse
  :: Monad m
  => Parser b
  -> PP.Producer ByteString m x
  -> m (Either PA.ParsingError b, PP.Producer ByteString m x)
parse p prod = do
  (mx, prod') <- PP.runStateT (PA.parse p) prod
  case mx of
    Nothing -> return (Left $ PA.ParsingError [] "no input!", prod')
    Just ex -> return (ex, prod')


data HeaderInfo = C | E | F | H | N | U deriving (Eq, Ord, Show, Read)


evtHeaderLine :: Parser (HeaderInfo, ByteString)
evtHeaderLine = do
  hi <- read . pure <$> satisfy (inClass "CEFHNU")
  bs <- skipSpace *> takeTill eol <* endOfLine
  return (hi, bs)

-- -- parse a vector of objects: first is the decimal length of the list
-- -- followed by the objects (separated by spaces)
-- vector :: Parser a -> Parser [a]
-- vector p = do
--     n <- decimal
--     replicateM n (skipSpace *> p)
