{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}


module Main where

import           Protolude             hiding (readFile, to, (<&>), (&))

import           Control.Monad.Catch   (MonadCatch, catchAll)
import           Data.Binary           (decode, encode)
import qualified Data.ByteString       as B
import           Data.Hashable         (hash)
import qualified Data.Text             as T
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           Lens.Micro
import           Lens.Micro.Mtl
import qualified System.Directory      as Dir
import           System.Posix.Files    (setFileMode)
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
  , imageCachePath             :: Text
  , usePrimarySelectionAsInput :: Bool
  , blacklistedApps            :: [Text]
  , trimSpaceFromSelection     :: Bool
  } deriving (Show, Read)

type ClipHistory = Vector Clip.Selection

readFile :: FilePath -> IO ByteString
readFile filepath = bracket (openFile filepath ReadMode) hClose $ \h -> do
  str <- hGetContents h
  return $! toS str


getHistory :: (MonadIO m, MonadReader Config m) => m ClipHistory
getHistory = do
  storePath <- view $ to (toS . historyPath)
  liftIO $ readH storePath `catchAll` const mempty
  where
    readH filePath = B.readFile filePath <&> V.fromList . decode . toS


getStaticHistory :: (MonadIO m, MonadReader Config m) => m ClipHistory
getStaticHistory = do
  storePath <- view $ to (toS . staticHistoryPath)
  liftIO $ readH storePath `catchAll` const mempty
  where
    readH filePath = readFile filePath <&> V.fromList . fmap toSelection . T.lines . toS
    toSelection txt = Clip.Selection "greenclip" (Clip.UTF8 txt)


storeHistory :: (MonadIO m, MonadReader Config m) => ClipHistory -> m ()
storeHistory history = do
  storePath <- view $ to (toS . historyPath)
  liftIO $ writeH storePath history
  where
    writeH storePath = B.writeFile storePath . toS . encode . V.toList


appendToHistory :: (MonadIO m, MonadReader Config m) => Clip.Selection -> ClipHistory -> m (ClipHistory, ClipHistory)
appendToHistory sel history' = do
  trimSelection <- view $ to trimSpaceFromSelection
  case sel of
    Clip.Selection appName (Clip.UTF8 txt) -> appendGeneric (if trimSelection then Clip.Selection appName (Clip.UTF8 (T.strip txt)) else sel) history'
    Clip.Selection _ (Clip.PNG bytes) -> appendImage Clip.PNG ".png" bytes
    Clip.Selection _ (Clip.JPEG bytes) -> appendImage Clip.JPEG ".jpeg" bytes
    Clip.Selection _ (Clip.BITMAP bytes) -> appendImage Clip.BITMAP ".bmp" bytes


  where
    appendImage imgCtr extension bytes = do
      cachePth <- view (to imageCachePath)
      let imgHash = show $ hash bytes
      let imgPath = toS $ cachePth <> imgHash <> extension
      _ <- liftIO $ writeImage imgPath bytes
      appendGeneric (sel {Clip.selection = imgCtr $ toS imgHash}) history'

    writeImage path bytes = do
      fileExist <- Dir.doesFileExist path
      if fileExist
        then return False
        else B.writeFile path bytes >> return True

    appendGeneric selection history =
      if maybe False (\sel' -> Clip.selection sel' == Clip.selection selection) (history V.!? 0)
        then return (history, mempty)
        else do
          maxLen <- view $ to maxHistoryLength
          return $ V.splitAt maxLen . V.cons selection $ V.filter (\ori -> Clip.selection ori /= Clip.selection selection) history


setHistoryFilePermission :: (MonadIO m, MonadReader Config m) => m ()
setHistoryFilePermission = do
  storePath <- view $ to (toS . historyPath)
  fileExist <- liftIO $ Dir.doesFileExist storePath
  when (not fileExist) (storeHistory mempty)
  liftIO $ setFileMode storePath 0o600

runDaemon:: (MonadIO m, MonadCatch m, MonadReader Config m) => m ()
runDaemon = setHistoryFilePermission >> (forever $ go `catchAll` handleError)
  where
    _0_5sec :: Int
    _0_5sec = 5 * 100000

    _5sec :: Int
    _5sec = 5000000

    go = do
      history <- getHistory
      usePrimary <- view $ to usePrimarySelectionAsInput
      cfg <- ask

      liftIO $ bracket Clip.getXorgContext Clip.destroyXorgContext $ \x11Context -> do
        let getSelections = (getSelectionFrom (Clip.getClipboardSelection x11Context), Nothing)
                          : [(getSelectionFrom (Clip.getPrimarySelection x11Context), Nothing) | usePrimary]
        void $ runReaderT (innerloop getSelections history) cfg

    getSelection [] = return ([], Nothing)
    getSelection ((getSel, lastSel):getSels) = do
      selection <- liftIO getSel
      if fmap Clip.selection selection /= fmap Clip.selection lastSel
         then return ((getSel, selection) : getSels, selection)
         else getSelection getSels >>= \(e, sel) -> return ((getSel, lastSel) : e, sel)

    innerloop :: (MonadIO m, MonadReader Config m) => [(IO (Maybe Clip.Selection), Maybe Clip.Selection)] -> ClipHistory -> m ClipHistory
    innerloop getSelections history = do
      -- Get selection from enabled clipboards
      (getSelections', sel) <- liftIO $ getSelection getSelections

      -- Do not use selection coming from blacklisted app
      liftIO $ when (isJust sel) (print (Clip.appName <$> sel))
      blacklist <- view (to blacklistedApps)
      let selection = sel >>= \s -> if isJust $ find (== Clip.appName s) blacklist
                                      then Nothing
                                      else Just s

      -- Append current selection to history and get back entries needed to be purged
      (history', toBePurged) <- maybe (return (history, mempty)) (`appendToHistory` history) selection
      traverse_ purgeSelection toBePurged

      -- backup on disk history if it as changed seen last backup
      when (isJust selection && history' /= history) (storeHistory history')

      -- Getting some rest
      liftIO $ threadDelay _0_5sec
      innerloop getSelections' history'

    getSelectionFrom :: IO (Maybe Clip.Selection) -> IO (Maybe Clip.Selection)
    getSelectionFrom = fmap join . timeout _5sec

    purgeSelection (Clip.Selection _ (Clip.PNG txt)) = purge (toS txt <> ".png")
    purgeSelection (Clip.Selection _ (Clip.JPEG txt)) = purge (toS txt <> ".jpeg")
    purgeSelection (Clip.Selection _ (Clip.BITMAP txt)) = purge (toS txt <> ".bmp")
    purgeSelection _ = return ()

    purge path = do
      cachePth <- view (to imageCachePath)
      liftIO $ Dir.removeFile (toS $ cachePth <> "/" <> path) `catchAll` const mempty

    handleError ex = do
      let displayMissing = "openDisplay" `T.isInfixOf` show ex
      if displayMissing
      then panic "X display not available. Please start Xorg before running greenclip"
      else print ex
      liftIO $ threadDelay _0_5sec


toRofiStr :: Clip.Selection -> Text
toRofiStr (Clip.Selection _ (Clip.UTF8 txt)) = T.map (\c -> if c == '\n' || c == '\r' then '\xA0' else c) txt
toRofiStr (Clip.Selection appName (Clip.PNG txt)) = "image/png " <> appName <> " " <> toS txt
toRofiStr (Clip.Selection appName (Clip.JPEG txt)) = "image/jpeg " <> appName <> " " <> toS txt
toRofiStr (Clip.Selection appName (Clip.BITMAP txt)) = "image/bmp " <> appName <> " " <> toS txt

fromRofiStr :: Text -> Text -> IO Clip.Selection
fromRofiStr cachePth txt@(T.isPrefixOf "image/png " -> True) = B.readFile (toS $ cachePth <> getHash txt <> ".png") <&> Clip.Selection "greenclip" . Clip.PNG
fromRofiStr cachePth txt@(T.isPrefixOf "image/jpeg " -> True) = B.readFile (toS $ cachePth <> getHash txt <> ".jpeg") <&> Clip.Selection "greenclip" . Clip.JPEG
fromRofiStr cachePth txt@(T.isPrefixOf "image/bmp " -> True) = B.readFile (toS $ cachePth <> getHash txt <> ".bmp") <&> Clip.Selection "greenclip" . Clip.BITMAP
fromRofiStr _ txt = return $ Clip.Selection "greenclip" (Clip.UTF8 (T.map (\c -> if c == '\xA0' then '\n' else c) txt))

getHash :: Text -> Text
getHash = fromMaybe mempty . lastMay . T.split (== ' ')


printHistoryForRofi :: (MonadIO m, MonadReader Config m) => m ()
printHistoryForRofi = do
  history <- mappend <$> getHistory <*> getStaticHistory
  _ <- traverse (putStrLn . toRofiStr) history
  return ()


advertiseSelection :: (MonadIO m, MonadReader Config m) => Text -> m ()
advertiseSelection txt = do
  cachePth <- view (to imageCachePath)
  selection <- liftIO $ fromRofiStr cachePth txt
  liftIO $ Clip.setClipboardSelection selection


getConfig :: IO Config
getConfig = do
  home <- Dir.getHomeDirectory
  let cfgPath = home <> "/.config/greenclip.cfg"

  cfgStr <- readFile cfgPath `catchAll` const mempty

  let unprettyCfg' = cfgStr & T.strip . T.replace "\n" "" . toS
  let unprettyCfg = if "trimSpaceFromSelection" `T.isInfixOf` unprettyCfg'
                      then unprettyCfg'
                      else T.replace "}" ", trimSpaceFromSelection = True }" unprettyCfg'
  let cfgMaybe = readMaybe $ toS unprettyCfg
  let cfg = fromMaybe defaultConfig cfgMaybe

  -- Write back the config file if the current one was invalid
  _ <- if isNothing cfgMaybe || unprettyCfg /= unprettyCfg'
        then let prettyCfg = show cfg & T.replace "," ",\n" . T.replace "{" "{\n " . T.replace "}" "\n}"
             in writeFile cfgPath prettyCfg
        else return ()

  historyPath' <- expandHomeDir $ cfg ^. to historyPath
  staticHistoryPath' <- expandHomeDir $ cfg ^. to staticHistoryPath
  imageCachePath' <- expandHomeDir $ cfg ^. to imageCachePath

  return $ cfg { historyPath = historyPath'
               , staticHistoryPath = staticHistoryPath'
               , imageCachePath = imageCachePath'
               }

  where
    defaultConfig = Config 50 "~/.cache/greenclip.history" "~/.cache/greenclip.staticHistory" "/tmp/" False [] True
    expandHomeDir str = (toS . fromMaybe (toS str) . listToMaybe <$> wordexp (toS str)) `catchAll` (\_ -> return $ toS str)


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
    COPY sel -> runReaderT (advertiseSelection sel) cfg
    HELP     -> putText $ "greenclip v3.1 -- Recyle your clipboard selections\n\n" <>
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
