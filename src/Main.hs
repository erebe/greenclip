{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Main where

import           ClassyPrelude    as CC
import           Data.Binary      (Binary, decode, encode, get, put)
import qualified Data.Text        as T
import qualified Data.Vector      as V
import qualified System.Clipboard as Clip


data Config = Config
  { maxHistory               :: Int
  , historyFilePath          :: String
  , permanentHistoryFilePath :: String
  }


type ClipHistory = Vector Text

instance Binary (Vector Text) where
  put v = put (length v) >> foldMap put v
  get = do
    len <- get
    history <- replicateM len get
    return $ fromList history



startDaemon :: Config -> IO ()
startDaemon Config{..} =
  go (mempty :: ClipHistory)
  where
    go history = do
      selection <- T.pack . fromMaybe mempty <$> Clip.getClipboardString

      let history' = appendToHistory selection history
      _ <- writeToFile history'

      _ <- threadDelay (10^(6 :: Int))
      go history'

    appendToHistory selection h =
      if selection /= fromMaybe mempty (headMay h)
        then fst $ V.splitAt maxHistory $ cons selection h
        else h

    writeToFile history = do
      writeFile historyFilePath (encode history)

main :: IO ()
main = do
  -- h <- decode <$> readFile "history.svg" :: IO (Vector Text)
  startDaemon (Config 5 "history.svg" mempty)
