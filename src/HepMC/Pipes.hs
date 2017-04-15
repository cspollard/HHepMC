{-# LANGUAGE RankNTypes #-}

module HepMC.Pipes where

import           Control.Exception.Base
import           Control.Monad          (void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.ByteString        (ByteString)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import           HepMC.Parse
import           Pipes                  ((>->))
import qualified Pipes                  as P
import qualified Pipes.Attoparsec       as PA
import qualified Pipes.ByteString       as PB
import qualified Pipes.Parse            as PP
import qualified Pipes.Prelude          as P
import           System.IO


-- fromFile :: (MonadIO m) => FilePath -> m ()
-- fromFile fname =
--   liftIO . withFile fname ReadMode $ \f -> do
--     (evers, p') <- parse hmcvers $ PB.fromHandle f
--     print evers
--
--     (m, ep'') <- hmceventheader p'
--
--     print m
--
--     case ep'' of
--       Right _ -> throwM $ PatternMatchFail "ran out of input!"
--       Left (_, p'') ->
--         void . P.runEffect
--         $ PA.parsed (hmcend <|> (takeTill eol <* endOfLine)) p''
--           >-> P.print


hmcevt
  :: Monad m
  => P.Producer ByteString m x
  -> P.Producer [ByteString] m (PA.ParsingError, P.Producer ByteString m x)
hmcevt p = do
  (_, (e, p')) <-
    lift
    $ P.fold' (flip $ uncurry M.insert) mempty id
      $ hepmcevtheader p

  (bs, p'') <- runStateT go p'
  hmcevent p''

  where
    go = do
      hmcvtx




hmcvtx :: Monad m => PP.Parser ByteString m (Either PA.ParsingError ByteString)
hmcvtx = parseOne $ do
  char 'V' *> skipSpace
  takeTill eol <* endOfLine

hmcpart :: Monad m => PP.Parser ByteString m (Either PA.ParsingError ByteString)
hmcpart = parseOne $ do
  char 'P' *> skipSpace
  takeTill eol <* endOfLine

hmcevtheader
  :: Monad m
  => PP.Parser ByteString m (Either PA.ParsingError (HeaderInfo, ByteString))
hmcevtheader = parseOne evtHeaderLine



parseOne
  :: Monad m
  => Parser b
  -> PP.Parser ByteString m (Either PA.ParsingError b)
parseOne p = do
  mex <- PA.parse p
  return $ fromMaybe (Left $ PA.ParsingError [] "no input!") mex
