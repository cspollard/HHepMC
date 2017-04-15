{-# LANGUAGE RankNTypes #-}

module HepMC.Pipes where

import           Control.Monad          (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.ByteString        (ByteString)
import           Data.Maybe             (fromMaybe)
import           HepMC.Event
import           HepMC.Parse
import qualified Pipes                  as P
import qualified Pipes.Attoparsec       as PA
import qualified Pipes.Parse            as PP

-- TODO
-- this doesn't exit cleanly....
fromStream :: MonadIO m => P.Producer ByteString m x -> P.Producer Event m ()
fromStream p = do
  (evers, p') <- lift $ PP.runStateT (parseOne hmcvers) p
  case evers of
    Left s -> liftIO $ print s
    Right v -> do
      liftIO . print $ "hepmc version: " ++ show v
      ex <- PA.parsed parserEvent p'
      case ex of
        Right _       -> liftIO $ print "no hepmc footer?!"
        Left (exx, p'') -> do
          liftIO $ print exx
          void . lift $ PP.runStateT (parseOne hmcend) p''


parseOne
  :: Monad m
  => Parser b
  -> PP.Parser ByteString m (Either PA.ParsingError b)
parseOne p = do
  mex <- PA.parse p
  return $ fromMaybe (Left $ PA.ParsingError [] "no input!") mex
