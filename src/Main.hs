{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Main where

import           Protolude             hiding (readFile, to)

import           Control.Monad.Catch   (MonadCatch, catchAll)
import           Data.Binary           (decode, encode)
import qualified Data.Text             as T
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           Lens.Micro
import           Lens.Micro.Mtl
import qualified System.Directory      as Dir
import           System.Environment    (lookupEnv)
import           System.IO             (IOMode (..), hClose, hGetContents,
                                        openFile)
import           System.Timeout        (timeout)
import           System.Wordexp.Simple (wordexp)

import qualified Clipboard             as Clip


data Command = DAEMON | PRINT | COPY Text | CLEAR | HELP deriving (Show, Read)

data Config = Config
  { maxHistoryLength           :: Int
  , historyPath                :: Text
  , staticHistoryPath          :: Text
  , usePrimarySelectionAsInput :: Bool
  } deriving (Show, Read)

type ClipHistory = Vector Text

readFile :: FilePath -> IO ByteString
readFile filepath = bracket (openFile filepath ReadMode) hClose $ \h -> do
  str <- hGetContents h
  return $! toS str


getHistory :: (MonadIO m, MonadReader Config m) => m ClipHistory
getHistory = do
  storePath <- view $ to (toS . historyPath)
  liftIO $ readH storePath
  where
    readH filePath = readFile filePath <&> V.fromList . decode . toS


getStaticHistory :: (MonadIO m, MonadReader Config m) => m ClipHistory
getStaticHistory = do
  storePath <- view $ to (toS . staticHistoryPath)
  liftIO $ readH storePath
  where
    readH filePath = readFile filePath <&> V.fromList . T.lines . toS


storeHistory :: (MonadIO m, MonadReader Config m) => ClipHistory -> m ()
storeHistory history = do
  storePath <- view $ to (toS . historyPath)
  liftIO $ writeH storePath history
  where
    writeH storePath = writeFile storePath . toS . encode . V.toList


appendToHistory :: (MonadIO m, MonadReader Config m) => Text -> ClipHistory -> m ClipHistory
appendToHistory sel history =
  let selection = T.strip sel in
  if selection == mempty || selection == fromMaybe mempty (V.headM history)
  then return history
  else do
    maxLen <- view $ to maxHistoryLength
    return $ fst . V.splitAt maxLen . V.cons selection $ V.filter (/= selection) history


runDaemon :: (MonadIO m, MonadCatch m, MonadReader Config m) => m ()
runDaemon = forever $ run `catchAll` handleError

  where
    _0_5sec :: Int
    _0_5sec = 5 * 100000

    _1sec :: Int
    _1sec = 1000000

    run = do
      history <- getHistory
      x11Context <- liftIO Clip.getXorgContext
      usePrimary <- view $ to usePrimarySelectionAsInput
      let getSelections = (getSelectionFrom (Clip.getClipboardSelection x11Context), mempty)
                        : [(getSelectionFrom (Clip.getPrimarySelection x11Context), mempty) | usePrimary]
      go getSelections history

    getSelection [] = return ([], mempty)
    getSelection ((getSel, lastSel):getSels) = do
      selection <- liftIO getSel
      if selection /= lastSel
         then return ((getSel, selection) : getSels, selection)
         else getSelection getSels >>= \(e, selection) -> return ((getSel, lastSel) : e, selection)

    go getSelections history = do
      (getSelections', selection) <- liftIO $ getSelection getSelections
      history' <- appendToHistory selection history
      when (history' /= history) (storeHistory history')

      liftIO $ threadDelay _0_5sec
      go getSelections' history'

    getSelectionFrom fn = do
      sel <- timeout _1sec fn
      case sel of
        Nothing -> return mempty
        Just _  -> return "ok"

    handleError ex = do
      let displayMissing = "openDisplay" `T.isInfixOf` show ex
      if displayMissing
      then panic "X display not available. Please start Xorg before running greenclip"
      else print ex
      liftIO $ threadDelay _0_5sec


toRofiStr :: Text -> Text
toRofiStr = T.map (\c -> if c == '\n' || c == '\r' then '\xA0' else c)

fromRofiStr :: Text -> Text
fromRofiStr = T.map (\c -> if c == '\xA0' then '\n' else c)


printHistoryForRofi :: (MonadIO m, MonadReader Config m) => m ()
printHistoryForRofi = do
  history <- mappend <$> getHistory <*> getStaticHistory
  _ <- traverse (putStrLn . toRofiStr) history
  return ()


advertiseSelection :: Text -> IO ()
advertiseSelection = undefined -- Clip.setClipboardString . fromRofiStr


getConfig :: IO Config
getConfig = do
  home <- Dir.getHomeDirectory
  let cfgPath = home <> "/.config/greenclip.cfg"

  cfgStr <- (readFile cfgPath <&> T.strip . toS) `catchAll` const mempty
  let cfg = fromMaybe (defaultConfig (toS home)) (readMaybe $ toS cfgStr)
  writeFile cfgPath (show cfg)

  historyPath' <- expandHomeDir . toS $ historyPath cfg
  staticHistoryPath' <- expandHomeDir . toS $ staticHistoryPath cfg

  let cfg' = cfg {historyPath = toS historyPath', staticHistoryPath = toS staticHistoryPath'}
  return cfg'

  where
    defaultConfig home = Config 25 (home <> "/.cache/greenclip.history") (home <> "/.cache/greenclip.staticHistory") False
    expandHomeDir str = (fromMaybe str . listToMaybe <$> wordexp str) `catchAll` (\_ -> return str)


parseArgs :: [Text] -> Command
parseArgs ("daemon":_)   = DAEMON
parseArgs ["clear"]      = CLEAR
parseArgs ["print"]      = PRINT
parseArgs ["print", sel] = COPY sel
parseArgs _              = HELP

run :: Command -> IO ()
run cmd = do
  cfg <- getConfig
  case cmd of
    DAEMON   -> runReaderT runDaemon cfg
    PRINT    -> runReaderT printHistoryForRofi cfg
    CLEAR    -> runReaderT (storeHistory mempty) cfg
    -- Should rename COPY into ADVERTISE but as greenclip is already used I don't want to break configs
    -- of other people
    COPY sel -> advertiseSelection sel
    HELP     -> putText $ "greenclip v2.1 -- Recyle your clipboard selections\n\n" <>
                          "Available commands\n" <>
                          "daemon: Spawn the daemon that will listen to selections\n" <>
                          "print:  Display all selections history\n" <>
                          "clear:  Clear history\n" <>
                          "help:   Display this message\n"

main :: IO ()
main = do
  displayPresent <- lookupEnv "DISPLAY"
  case displayPresent of
    Nothing -> putText "X display not available. Please start Xorg before running greenclip"
    _       -> getArgs >>= run . parseArgs . fmap toS
