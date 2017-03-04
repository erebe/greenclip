{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           ClassyPrelude      as CP hiding (readFile)
import           Data.Binary        (decode, encode)
import qualified Data.Text          as T
import qualified Data.Vector        as V
import           Lens.Micro
import           Lens.Micro.Mtl
import qualified System.Clipboard   as Clip
import qualified System.Directory   as Dir
import           System.IO          (IOMode (..), openFile)
import           System.Environment          (lookupEnv)


data Command = DAEMON | PRINT | COPY Text | HELP deriving (Show, Read)

data Config = Config
  { maxHistoryLength  :: Int
  , historyPath       :: String
  , staticHistoryPath :: String
  } deriving (Show, Read)


type ClipHistory = Vector Text

readFile :: FilePath -> IO ByteString
readFile filepath = bracket (openFile filepath ReadMode) hClose $ \h -> do
  str <- hGetContents h
  let !str' = str
  return str'


getHistory :: (MonadIO m, MonadReader Config m) => m ClipHistory
getHistory = do
  storePath <- view (to historyPath)
  liftIO $ readH storePath
  where
    readH filePath = (readFile filePath <&> fromList . decode . fromStrict) `catchAnyDeep` const mempty

getStaticHistory :: (MonadIO m, MonadReader Config m) => m ClipHistory
getStaticHistory = do
  storePath <- view (to staticHistoryPath)
  liftIO $ readH storePath
  where
    readH filePath = (readFile filePath <&> fromList . T.lines . decodeUtf8) `catchAnyDeep` const mempty

storeHistory :: (MonadIO m, MonadReader Config m) => ClipHistory -> m ()
storeHistory history = do
  storePath <- view (to historyPath)
  liftIO $ writeFile storePath (toStrict . encode . toList $ history) `catchAnyDeep` const mempty

appendH :: (MonadIO m, MonadReader Config m) => Text -> ClipHistory -> m ClipHistory
appendH sel history =
  let selection = T.strip sel in
  if selection == mempty
     || selection == fromMaybe mempty (headMay history)
  then return history
  else do
    maxLen <- view (to maxHistoryLength)
    return $ fst . V.splitAt maxLen $ cons selection $ filter (/= selection) history

runDaemon :: (MonadIO m, MonadReader Config m, MonadCatch m) => m ()
runDaemon = forever $ (getHistory >>= go) `catchAnyDeep` handleError
  where
    _1sec :: Int
    _1sec = 5 * 10^(5::Int)

    go history = do
      selection <- liftIO getSelection

      history' <- appendH selection history
      when (history' /= history) (storeHistory history')

      liftIO $ threadDelay _1sec
      go history'

    getSelection :: IO Text
    getSelection = T.pack . fromMaybe mempty <$> Clip.getClipboardString

    handleError ex = do
      let displayMissing = "openDisplay" `T.isInfixOf` (tshow ex)
      case displayMissing of
        True -> error "X display not available. Please start Xorg before running greenclip" 
        _ -> sayErrShow ex
             

printHistory :: (MonadIO m, MonadReader Config m) => m ()
printHistory = do
  history <- mappend <$> getHistory <*> getStaticHistory
  _ <- traverse (putStrLn . T.dropEnd 1 . T.drop 1 . tshow) history
  return ()


getConfig :: IO Config
getConfig = do
  home <- Dir.getHomeDirectory
  let cfgPath = home </> ".config/greenclip.cfg"

  cfgStr <- (readFile cfgPath <&> decodeUtf8) `catchAnyDeep` const mempty
  let cfg = fromMaybe (defaultConfig home) (readMay cfgStr)

  writeFile cfgPath (fromString $ show cfg)
  return cfg

  where
    defaultConfig home = Config 15 (home </> ".cache/greenclip.history") (home </> ".cache/greenclip.staticHistory")

pasteSelection :: Text -> IO ()
pasteSelection sel = Clip.setClipboardString (fromMaybe mempty (readMay $ "\"" <> sel <> "\""))

parseArgs :: [Text] -> Command
parseArgs ("daemon":_)   = DAEMON
parseArgs ["print"]      = PRINT
parseArgs ["print", sel] = COPY sel
parseArgs _              = HELP

run :: Command -> IO ()
run cmd = do
  cfg <- getConfig
  case cmd of
    DAEMON   -> runReaderT runDaemon cfg
    PRINT    -> runReaderT printHistory cfg
    COPY sel -> pasteSelection sel
    HELP     -> putStrLn $ "daemon: to spawn the daemon that will listen to selections\n" <>
                           "print: To display all selections history"

main :: IO ()
main = do
  displayPresent <- lookupEnv "DISPLAY"
  case displayPresent of
    Nothing -> putStrLn "X display not available. Please start Xorg before running greenclip"
    _ -> do cmd <- parseArgs <$> getArgs
            run cmd
