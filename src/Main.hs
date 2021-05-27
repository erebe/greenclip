
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}


module Main where

import           Protolude             hiding (readFile, to, (<&>), (&), toS)
import           Protolude.Conv        (toS)

import           Control.Monad.Catch   (MonadCatch, catchAll)
import           Data.Binary           (decodeFile, encode)
import qualified Data.ByteString       as B
import           Data.List             (dropWhileEnd)
import qualified Data.Text             as T
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           Lens.Micro
import           Lens.Micro.Mtl    hiding ((.=))
import qualified System.Directory      as Dir
import           System.Posix.Files    (setFileMode)
import           System.Environment    (lookupEnv)
import           System.IO             (hClose, hGetContents)
import           System.Timeout        (timeout)
import           System.Wordexp.Simple (wordexp)
--import           System.Posix.Temp     (mkdtemp)

import Toml (TomlCodec, (.=))
import qualified Toml

import qualified Clipboard             as Clip


data Command = DAEMON | PRINT | COPY Text | CLEAR | HELP deriving (Show, Read)

data Config = Config
  { maxHistoryLength           :: Int
  , maxItemSizeBytes           :: Int
  , historyPath                :: Text
  , imageCachePath             :: Text
  , usePrimarySelectionAsInput :: Bool
  , blacklistedApps            :: [Text]
  , trimSpaceFromSelection     :: Bool
  , enableImageSupport         :: Bool
  , staticHistory              :: [Text]
  } deriving (Show, Read)

configCodec :: TomlCodec Config
configCodec = Config
    <$> Toml.int "max_history_length"  .= maxHistoryLength
    <*> Toml.int "max_selection_size_bytes" .= maxItemSizeBytes
    <*> Toml.text "history_file" .= historyPath
    <*> Toml.text "image_cache_directory" .= imageCachePath
    <*> Toml.bool "use_primary_selection_as_input" .= usePrimarySelectionAsInput
    <*> Toml.arrayOf Toml._Text  "blacklisted_applications" .= blacklistedApps
    <*> Toml.bool "trim_space_from_selection" .= trimSpaceFromSelection
    <*> Toml.bool "enable_image_support" .= enableImageSupport
    <*> Toml.arrayOf Toml._Text  "static_history" .= staticHistory


type ClipHistory = Vector Clip.Selection

readFile :: FilePath -> IO ByteString
readFile filepath = bracket (openFile filepath ReadMode) hClose $ \h -> do
  str <- hGetContents h
  return $! toS str


getHistory :: (MonadIO m, MonadReader Config m) => m ClipHistory
getHistory = do
  storePath <- view $ to (toS . historyPath)
  liftIO $ (V.fromList <$> decodeFile storePath) `catchAll` const mempty


getStaticHistory :: (MonadIO m, MonadReader Config m) => m ClipHistory
getStaticHistory = do
  history <- view $ to (staticHistory)
  return . V.fromList $ Clip.Selection "greenclip" . Clip.UTF8 <$> history



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
      let imgPath = toS $ cachePth <> "/" <> imgHash <> extension
      _ <- liftIO $ writeImage imgPath bytes
      appendGeneric (sel {Clip.selection = imgCtr $ toS imgHash}) history'

    writeImage path bytes = do
      fileExist <- Dir.doesFileExist path
      if fileExist
        then return False
        else B.writeFile path bytes >> setFileMode path 0o600 >> return True

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

prepareDirs :: (MonadIO m, MonadReader Config m) => m ()
prepareDirs = do
  historyFile <- view $ to (T.unpack . historyPath)
  imgDir <- view $ to (T.unpack . imageCachePath)
  let dirs = [imgDir, dropWhileEnd (/= '/') historyFile]
  mapM_ (liftIO . Dir.createDirectoryIfMissing True) dirs

runDaemon:: (MonadIO m, MonadCatch m, MonadReader Config m) => m ()
runDaemon = prepareDirs >> setHistoryFilePermission >> (forever $ go `catchAll` handleError)
  where
    _0_5sec :: Int
    _0_5sec = 5 * 100000

    _5sec :: Int
    _5sec = 5000000

    go = do
      history <- getHistory
      usePrimary <- view $ to usePrimarySelectionAsInput
      enableImage <- view $ to enableImageSupport
      cfg <- ask

      liftIO $ bracket Clip.getXorgContext Clip.destroyXorgContext $ \x11Context -> do
        let getSelections = (getSelectionFrom (Clip.getClipboardSelection x11Context enableImage), Nothing)
                          : [(getSelectionFrom (Clip.getPrimarySelection x11Context enableImage), Nothing) | usePrimary]
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
      (getSelections', rawSelection) <- liftIO $ getSelection getSelections

      -- Do not store selection items above threshold size
      maxItemSize <- view (to maxItemSizeBytes)
      let sel = case rawSelection of
            Nothing -> Nothing
            Just selection -> if maxItemSize > 0 && Clip.selectionLength selection >= maxItemSize
                              then Nothing
                              else Just selection
             
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
fromRofiStr cachePth txt@(T.isPrefixOf "image/png " -> True) = B.readFile (toS $ cachePth <> "/" <> getHash txt <> ".png") <&> Clip.Selection "greenclip" . Clip.PNG
fromRofiStr cachePth txt@(T.isPrefixOf "image/jpeg " -> True) = B.readFile (toS $ cachePth <> "/" <> getHash txt <> ".jpeg") <&> Clip.Selection "greenclip" . Clip.JPEG
fromRofiStr cachePth txt@(T.isPrefixOf "image/bmp " -> True) = B.readFile (toS $ cachePth <> "/" <> getHash txt <> ".bmp") <&> Clip.Selection "greenclip" . Clip.BITMAP
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
  let configPath = home <> "/.config/greenclip.toml"
  
  configExist <- Dir.doesFileExist configPath
  when (not configExist) $ do 
    config <- Toml.encode (Toml.table configCodec "greenclip") <$> defaultConfig
    writeFile configPath config
    return ()
  
  tomlRes <- Toml.decodeFileEither (Toml.table configCodec "greenclip") configPath
  when (isLeft tomlRes) $ do
    die . toS $  "Error parsing the config file at " <> (show configPath) <> "\n" <> Toml.prettyTomlDecodeErrors (fromLeft mempty tomlRes)
  
  let cfg = fromRight (Config 50 0 "" "" False [] True True []) tomlRes 
  
  -- Replace $HOME|~|*... in config path
  cfg <- do
    imgCachePath <- wordexp . toS $ imageCachePath cfg
    historyP <- wordexp . toS $ historyPath cfg 
    return $ cfg { imageCachePath = (toS $ headDef "" imgCachePath), historyPath = (toS $ headDef "" historyP)}
    
  -- if it ends with / we don't create a temp directory
  -- user is responsible for it
  -- cfg <- if (lastDef ' ' (toS $ imageCachePath cfg) /= '/')
  --    then do
  --      dirPath <- mkdtemp $ (toS $ imageCachePath cfg)
  --      return $ cfg { imageCachePath = toS dirPath }
  --    else return cfg
     
  return cfg

  where
    defaultConfig = do 
      homeDir <- toS . fromMaybe mempty . listToMaybe <$> wordexp "~/"
      return $ Config 50 0 (homeDir <> ".cache/greenclip.history") "/tmp/greenclip" False [] True True 
        ["Greenclip has been updated to v4.1, update your new config file at ~/.config/greenclip.toml"]


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
    HELP     -> putText $ "greenclip v4.2 -- Recyle your clipboard selections\n\n" <>
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
