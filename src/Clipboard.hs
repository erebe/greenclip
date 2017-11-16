{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Clipboard where

import           Protolude

import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras

import           Lens.Micro               ((<&>))
import qualified Data.ByteString as B
import           Control.Concurrent       (threadDelay)

-- import           System.Posix.Process     (forkProcess)
-- import           System.Directory         (setCurrentDirectory)
-- import           System.IO                (hClose, stderr, stdin, stdout)

-- import           Foreign.C.Types          (CChar, CUChar)
-- import           Foreign.Marshal.Array    (withArrayLen)
import           Foreign                  (alloca, castPtr, peek,
                                           peekArray, {- peekByteOff -})

data SelectionType = UTF8 Text
                   | PNG ByteString
                   | JPEG ByteString
                   | BITMAP ByteString
                   deriving (Show)

data Selection = Selection {
    appName   :: Text
  , selection :: SelectionType
} deriving (Show)

data XorgContext = XorgContext {
    display          :: Display
  , ownWindow        :: Window
  , defaultClipboard :: Atom
  , primaryClipboard :: Atom
  , selectionTarget  :: Atom
} deriving (Show)


test :: IO ()
test = do
  ctx <- getXorgContext

  ret <- getClipboardSelection ctx
  print $! ret

  return ()

windowNameOfClipboardOwner :: XorgContext -> Atom -> IO Text
windowNameOfClipboardOwner XorgContext{..} clipboard = do

  window <- xGetSelectionOwner display clipboard
  windowName <- fetchName display window <&> toS . fromMaybe mempty
  return $! windowName


getSupportedMimes :: XorgContext -> Atom -> IO [Text]
getSupportedMimes ctx@XorgContext{..} clipboard =
    alloca $ \actual_type_return ->
    alloca $ \actual_format_return ->
    alloca $ \nitems_return ->
    alloca $ \bytes_after_return ->
    alloca $ \prop_return -> do
        targets <- internAtom display "TARGETS" False
        xConvertSelection display clipboard targets selectionTarget ownWindow currentTime
        _ <- waitNotify ctx
        ret <- xGetWindowProperty display ownWindow selectionTarget 0 0xFFFFFFFF False aTOM
                           actual_type_return
                           actual_format_return
                           nitems_return
                           bytes_after_return
                           prop_return

        ret2 <- if ret /= 0
                then return Nothing
                else do
                    prop_ptr      <- peek prop_return
                    actual_format <- peek actual_format_return <&> fromIntegral :: IO Atom
                    nitems        <- peek nitems_return <&> fromIntegral
                    getprop prop_ptr nitems actual_format
        print $ ret2
        fmap (fmap toS) (getAtomNames display $ fromMaybe mempty ret2)
  where
    getprop prop_ptr nitems actual_format
        | actual_format == 0    = return Nothing -- Property not found
        | otherwise = do
            retval <- peekArray nitems (castPtr prop_ptr)
            _ <- xFree prop_ptr
            return $ Just retval


getClipboardSelection :: XorgContext -> IO Selection
getClipboardSelection ctx@XorgContext{..} =
  getSelection ctx defaultClipboard

getPrimarySelection :: XorgContext -> IO Selection
getPrimarySelection ctx@XorgContext{..} =
  getSelection ctx primaryClipboard

getSelection :: XorgContext -> Atom -> IO Selection
getSelection ctx@XorgContext{..} clipboard = do
  mimes <- getSupportedMimes ctx clipboard
  print mimes
  let selectedMime = chooseSelectionType mimes

  target <- internAtom display (toS selectedMime) True
  xConvertSelection display clipboard target selectionTarget ownWindow currentTime
  waitNotify ctx

  clipboardContent <- getWindowProperty8 display selectionTarget ownWindow
                      <&> B.pack . map fromIntegral . fromMaybe mempty

  windowName <- windowNameOfClipboardOwner ctx clipboard
  return Selection {
    appName = windowName
  , selection = mimeToSelectionType selectedMime clipboardContent
  }

 where
   priorities = ["image/png", "image/jpeg", "image/bmp", "UTF8_STRING", "TEXT"]

   chooseSelectionType mimes =
     let selectedMime = msum $ (\mime -> find (== mime) mimes) <$> priorities
     in fromMaybe "UTF8_STRING" selectedMime

   mimeToSelectionType "image/png" selContent = PNG selContent
   mimeToSelectionType "image/jpeg" selContent = JPEG selContent
   mimeToSelectionType "image/bmp" selContent = BITMAP selContent
   mimeToSelectionType _ selContent = UTF8 (toS selContent)


getXorgContext :: IO XorgContext
getXorgContext = do
    display <- openDisplay mempty
    window <- createSimpleWindow display (defaultRootWindow display) 0 0 1 1 0 0 0
    clipboard <- internAtom display "CLIPBOARD" False
    selTarget <- internAtom display "GREENCLIP" False
    return XorgContext {
        display = display
      , ownWindow = window
      , defaultClipboard = clipboard
      , primaryClipboard = pRIMARY
      , selectionTarget = selTarget
    }

destroyXorgContext :: XorgContext -> IO ()
destroyXorgContext XorgContext{..} = do
    destroyWindow display ownWindow
    closeDisplay display

waitNotify :: XorgContext -> IO ()
waitNotify XorgContext{..} = allocaXEvent (go display ownWindow)
  where
  go display' window evPtr = do
    waitForEvents display'
    nextEvent display' evPtr
    ev <- getEvent evPtr
    when (ev_event_type ev /= selectionNotify)
      (go display' window evPtr)

  waitForEvents display' = do
    nbEvs <- pending display'
    when (nbEvs == 0) $ threadDelay _1ms >> waitForEvents display'

  _1ms = 100000








-- getClipboardString :: IO (Maybe Text)
-- getClipboardString = do
--     (display, window, clipboards) <- readIORef initialSetup
--
--     inp <- internAtom display "clipboard_get" False
--     target <- internAtom display "UTF8_STRING" True
--     xConvertSelection display (unsafeHead clipboards) target inp window currentTime
--     Just <$> waitClipboardNotify display window inp
--
-- getPrimaryClipboard :: IO (Maybe Text)
-- getPrimaryClipboard = do
--     (display, window, clipboards) <- readIORef initialSetup
--     inp <- internAtom display "clipboard_get" False
--     target <- internAtom display "UTF8_STRING" True
--     xConvertSelection display (unsafeIndex clipboards 1) target inp window currentTime
--     Just <$> waitClipboardNotify display window inp
--
-- waitClipboardNotify :: Display -> Window -> Atom -> IO Text
-- waitClipboardNotify display' window' inp' = allocaXEvent (go display' window' inp')
--   where
--   go display window inp evPtr = do
--     waitForEvent display
--     nextEvent display evPtr
--     ev <- getEvent evPtr
--     if ev_event_type ev == selectionNotify
--        then charsToString . fromMaybe mempty <$> getWindowProperty8 display inp window
--        else go display window inp evPtr
--   waitForEvent display = do
--     nbEvs <- pending display
--     when (nbEvs == 0) $ threadDelay 100000 >> waitForEvent display
--
-- charsToString :: [CChar] -> Text
-- charsToString = toS . decode . map fromIntegral
--
-- setClipboardString :: Text -> IO ()
-- setClipboardString str = void $ forkProcess $ do
--         mapM_ hClose [stdin, stdout, stderr]
--         setCurrentDirectory "/"
--         (display, window, clipboards) <- readIORef initialSetup
--         mapM_ (\atom -> xSetSelectionOwner display atom window currentTime) clipboards
--         advertiseSelection display clipboards (stringToChars str)
--         cleanup display window
--
-- advertiseSelection :: Display -> [Atom] -> [CUChar] -> IO ()
-- advertiseSelection display clipboards' str = allocaXEvent (go clipboards')
--   where
--     go [] _ = return ()
--     go clipboards evPtr = do
--       nextEvent display evPtr
--       ev <- getEvent evPtr
--       case ev of
--           SelectionRequest {..} -> do
--               target' <- getAtomName display ev_target
--               res <- handleOutput display ev_requestor ev_property (toS <$> target') str
--               sendSelectionNotify display ev_requestor ev_selection ev_target res ev_time
--               go clipboards evPtr
--
--           _ | ev_event_type ev == selectionClear -> do
--               target <- peekByteOff evPtr 40 :: IO Atom
--               go (filter (/= target) clipboards) evPtr
--
--           _ -> go clipboards evPtr
--
-- handleOutput :: Display -> Window -> Atom -> Maybe Text -> [CUChar] -> IO Atom
-- handleOutput display req prop (Just "UTF8_STRING") str = do
--     prop' <- getAtomName display prop
--     if isNothing prop' then handleOutput display req prop Nothing str else do
--         target <- internAtom display "UTF8_STRING" True
--         void $ withArrayLen str $ \len str' ->
--             xChangeProperty display req prop target 8 propModeReplace str'
--                             (fromIntegral len)
--         return prop
-- handleOutput _ _ _ _ _ = return none
--
-- sendSelectionNotify :: Display -> Window -> Atom -> Atom -> Atom -> Time -> IO ()
-- sendSelectionNotify display req sel target prop time = allocaXEvent $ \ev -> do
--     setEventType ev selectionNotify
--     setSelectionNotify ev req sel target prop time
--     sendEvent display req False 0 ev
--
-- stringToChars :: Text -> [CUChar]
-- stringToChars = map fromIntegral . encode . toS
--
-- initialSetup :: IORef (Display, Window, [Atom])
-- initialSetup = unsafePerformIO $ do
--     display <- openDisplay ""
--     window <- createSimpleWindow display (defaultRootWindow display)
--                                  0 0 1 1 0 0 0
--     clipboards <- internAtom display "CLIPBOARD" False
--     newIORef (display, window, [clipboards, pRIMARY])
-- {-# NOINLINE initialSetup #-}
--
--
-- cleanup :: Display -> Window -> IO ()
-- cleanup display window = do
--     destroyWindow display window
--     closeDisplay display
