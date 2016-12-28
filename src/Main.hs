{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           ClassyPrelude    as CC
import           Data.Binary      (Binary, decode, encode, get, put)
import qualified Data.Text        as T
import qualified Data.Vector      as V
import           Lens.Micro.Mtl
import           Lens.Micro.TH (makeLenses)
import qualified System.Clipboard as Clip


data Config = Config
  { _maxHistoryLength  :: Int
  , _historyPath       :: String
  , _staticHistoryPath :: String
  } deriving (Show, Read)

makeLenses ''Config

type ClipHistory = Vector Text
type CCC = ReaderT Config IO


getHistory ::(MonadIO m, MonadReader Config m) => m (ClipHistory)
getHistory = do
  storePath <- view historyPath
  history <- liftIO $ fromList . decode <$> readFile storePath
  liftIO $ print history
  return history

storeHistory :: (MonadIO m, MonadReader Config m) => ClipHistory -> m ()
storeHistory history = do
  storePath <- view historyPath
  liftIO $ writeFile storePath (encode . toList $ history)

-- startDaemon :: Config -> IO ()
-- startDaemon cfg@Config{..} =
--   go (mempty :: ClipHistory)
--   where
--     go history = do
--       selection <- T.pack . fromMaybe mempty <$> Clip.getClipboardString

--       history' <- appendToHistory selection history

--       _ <- threadDelay (10^(6 :: Int))
--       go history'

--     appendToHistory selection h =
--       if selection /= fromMaybe mempty (headMay h)
--         then do
--           let history' = fst . V.splitAt maxHistoryLength $ cons selection h
--           _ <- storeHistory cfg history'
--           return history'
--         else return h

main :: IO ()
main = do
  -- startDaemon (Config 5 "history.svg" mempty)
  undefined
