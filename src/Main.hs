{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           ClassyPrelude    as CP hiding (readFile)
import           Data.Binary      (decode, encode)
import qualified Data.Text        as T
import qualified Data.Vector      as V
import           Lens.Micro
import           Lens.Micro.Mtl
import qualified System.Clipboard as Clip
import qualified System.Directory as Dir
import           System.IO        (IOMode (..), openFile)


data Command = DAEMON | PRINT | COPY Text | HELP deriving (Show, Read)

data Config = Config
  { maxHistoryLength  :: Int
  , historyPath       :: String
  , staticHistoryPath :: String
  } deriving (Show, Read)


type ClipHistory = Vector Text
type CCC = ReaderT Config IO


readFile :: IOData str => FilePath -> IO str
readFile filepath = bracket (openFile filepath ReadMode) (hClose) $ \h -> do
  str <- hGetContents h
  let !str' = str
  return str'


getHistory :: (MonadIO m, MonadReader Config m) => m ClipHistory
getHistory = do
  storePath <- view (to historyPath)
  liftIO $ readH storePath
  where
    readH filePath = (readFile filePath <&> fromList . decode) `catchAnyDeep` (const mempty)

getStaticHistory :: (MonadIO m, MonadReader Config m) => m ClipHistory
getStaticHistory = do
  storePath <- view (to staticHistoryPath)
  liftIO $ readH storePath
  where
    readH filePath = (readFile filePath <&> fromList . T.lines) `catchAnyDeep` (const mempty)

storeHistory :: (MonadIO m, MonadReader Config m) => ClipHistory -> m ()
storeHistory history = do
  storePath <- view (to historyPath)
  liftIO $ writeFile storePath (encode . toList $ history) `catchAnyDeep` (const mempty)

appendH :: (MonadIO m, MonadReader Config m) => Text -> ClipHistory -> m ClipHistory
appendH sel history =
  let selection = T.strip sel in
  if selection == mempty
     || selection == fromMaybe mempty (headMay history)
  then return $ history
  else do
    maxLen <- view (to maxHistoryLength)
    return $ fst . V.splitAt maxLen $ cons selection $ filter (/= selection) history

runDaemon :: (MonadIO m, MonadReader Config m, MonadCatch m) => m ()
runDaemon = forever $ (getHistory >>= go) `catchAnyDeep` sayErrShow
  where
    _1sec :: Int
    _1sec = 5 * 10^5 

    go history = do
      selection <- liftIO $ getSelection

      history' <- appendH selection history
      when (history' /= history) (storeHistory history')

      liftIO $ threadDelay (10^(6 :: Int))
      go history'

    getSelection :: IO Text
    getSelection = T.pack . fromMaybe mempty <$> Clip.getClipboardString

printHistory :: (MonadIO m, MonadReader Config m) => m ()
printHistory = do
  history <- mappend <$> getHistory <*> getStaticHistory
  traverse (putStrLn . T.dropEnd 1 . T.drop 1 . tshow) history
  return ()


getConfig :: IO Config
getConfig = do
  home <- Dir.getHomeDirectory
  let cfgPath = home </> ".config/mclip.cfg"

  cfgStr <- readFile cfgPath `catchAnyDeep` (const mempty) :: IO Text
  let cfg = fromMaybe (defaultConfig home) (readMay cfgStr)

  writeFile cfgPath (show cfg)
  return cfg

  where
    defaultConfig home = Config 25 (home </> ".cache/mclip.history") (home </> ".cache/mclip.staticHistory")

pasteSelection :: Text -> IO ()
pasteSelection sel = Clip.setClipboardString (fromMaybe mempty (readMay $ "\"" <> sel <> "\""))

parseArgs :: [Text] -> Command
parseArgs ("daemon":_)     = DAEMON
parseArgs ("print":[])     = PRINT
parseArgs ("print":sel:[]) = COPY sel
parseArgs _                = HELP

run :: Command -> IO ()
run cmd = do
  cfg <- getConfig
  case cmd of
    DAEMON   -> runReaderT runDaemon cfg
    PRINT    -> runReaderT printHistory cfg
    COPY sel -> pasteSelection sel
    HELP     -> print "HELP"

main :: IO ()
main = do
  cmd <- parseArgs <$> getArgs
  run cmd
