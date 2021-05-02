{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Clipboard where

import           Protolude                hiding ((<&>))
import           Protolude.Unsafe                   (unsafeIndex)
import qualified Protolude.Conv as StrConvOld

import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras

import           Data.Binary              (Binary)
import qualified Data.ByteString          as B
import qualified Data.Text as T
import           Lens.Micro

import           System.Directory         (setCurrentDirectory)
import           System.IO                (hClose)
import           System.Posix.Process     (forkProcess)

import           Data.ByteString          (unpack)
import           Foreign                  (alloca, castPtr, peek, peekArray)
import           Foreign.C.Types          (CUChar)
import           Foreign.Marshal.Array    (withArrayLen)

data SelectionType = UTF8 Text
                   | PNG ByteString
                   | JPEG ByteString
                   | BITMAP ByteString
                   deriving (Show, Eq, Generic, Binary)

selectionLength :: Selection -> Int
selectionLength (Selection _ (UTF8 a)) = T.length a
selectionLength (Selection _ (PNG a)) = B.length a
selectionLength (Selection _ (JPEG a)) = B.length a
selectionLength (Selection _ (BITMAP a)) = B.length a


data Selection = Selection {
    appName   :: Text
  , selection :: SelectionType
} deriving (Show, Eq, Generic, Binary)

data XorgContext = XorgContext {
    display          :: Display
  , ownWindow        :: Window
  , defaultClipboard :: Atom
  , primaryClipboard :: Atom
  , selectionTarget  :: Atom
  , mimesPriorities  :: [Atom]
  , defaultMime      :: Atom
} deriving (Show)

test :: IO ()
test =
  bracket getXorgContext destroyXorgContext $ \ctx -> do
    ret <- getPrimarySelection ctx True
    print $! ret
    return ()

windowNameOfClipboardOwner :: XorgContext -> Atom -> IO Text
windowNameOfClipboardOwner XorgContext{..} clipboard = do

  window <- xGetSelectionOwner display clipboard

  if window > 0
    then fetchName display window <&> toS . fromMaybe mempty
    else return mempty


isIncrementalTransfert :: XorgContext -> IO Bool
isIncrementalTransfert XorgContext{..}  =
    alloca $ \actual_type_return ->
    alloca $ \actual_format_return ->
    alloca $ \nitems_return ->
    alloca $ \bytes_after_return ->
    alloca $ \prop_return -> do
        incr <- internAtom display "INCR" False
        ret <- xGetWindowProperty display ownWindow selectionTarget 0 0 False anyPropertyType
                           actual_type_return
                           actual_format_return
                           nitems_return
                           bytes_after_return
                           prop_return

        if ret /= 0
           then return False
        else do
            actual_type   <- peek actual_type_return <&> fromIntegral :: IO Atom
            _ <- peek prop_return >>= xFree
            return $ actual_type == incr

getSupportedMimes :: XorgContext -> Atom -> IO [Atom]
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
        return $ fromMaybe mempty ret2

  where
    getprop prop_ptr nitems actual_format
        | actual_format == 0    = return Nothing -- Property not found
        | otherwise = do
            retval <- peekArray nitems (castPtr prop_ptr)
            _ <- xFree prop_ptr
            return $ Just retval


getClipboardSelection :: XorgContext -> Bool -> IO (Maybe Selection)
getClipboardSelection ctx@XorgContext{..} enableImage =
  getSelection ctx enableImage defaultClipboard

getPrimarySelection :: XorgContext -> Bool -> IO (Maybe Selection)
getPrimarySelection ctx@XorgContext{..} enableImage =
  getSelection ctx enableImage primaryClipboard

getSelection :: XorgContext -> Bool -> Atom -> IO (Maybe Selection)
getSelection ctx@XorgContext{..} enableImage clipboard = do
  mimes <- if enableImage 
            then getSupportedMimes ctx clipboard 
            else return [defaultMime] 
  let targetMime = chooseSelectionType mimes

  xConvertSelection display clipboard targetMime selectionTarget ownWindow currentTime
  waitNotify ctx
  isIncremental <- isIncrementalTransfert ctx
  clipboardContent <- if isIncremental
                      then return mempty -- Incremental use too much CPU, do not handle it
                      else getWindowProperty8 display selectionTarget ownWindow
                           <&> B.pack . map fromIntegral . fromMaybe mempty

  if clipboardContent == mempty
  then return Nothing
  else do
    windowName <- windowNameOfClipboardOwner ctx clipboard
    return $ Just Selection { appName = windowName
                            , selection = mimeToSelectionType targetMime clipboardContent
                            }

 where
   chooseSelectionType mimes =
     let selectedMime = msum $ (\mime -> find (== mime) mimes) <$> mimesPriorities
     in fromMaybe defaultMime selectedMime

   mimeToSelectionType mimeTarget selContent =
     if      mimeTarget == unsafeIndex mimesPriorities 0 then PNG selContent
     else if mimeTarget == unsafeIndex mimesPriorities 1 then JPEG selContent
     else if mimeTarget == unsafeIndex mimesPriorities 2 then BITMAP selContent
     else UTF8 $ decodeUtf8 selContent

   -- getContentIncrementally acc = do
   --   _ <- xDeleteProperty display ownWindow selectionTarget
   --   flush display
   --   waitNotify ctx
   --   content <- getWindowProperty8 display selectionTarget ownWindow
   --              <&> B.pack . map fromIntegral . fromMaybe mempty
   --   if content == mempty
   --      then return acc
   --      else getContentIncrementally (acc <> content)


getXorgContext :: IO XorgContext
getXorgContext = do
    display <- openDisplay mempty
    window <- createSimpleWindow display (defaultRootWindow display) 0 0 1 1 0 0 0
    -- selectInput display window propertyChangeMask

    clipboard <- internAtom display "CLIPBOARD" False
    selTarget <- internAtom display "GREENCLIP" False
    priorities <- traverse (\atomName -> internAtom display atomName False) ["image/png", "image/jpeg", "image/bmp", "UTF8_STRING", "TEXT"] 
    defaultM <- internAtom display "UTF8_STRING" False
    return XorgContext {
        display = display
      , ownWindow = window
      , defaultClipboard = clipboard
      , primaryClipboard = pRIMARY
      , selectionTarget = selTarget
      , mimesPriorities = priorities
      , defaultMime = defaultM
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
    when (ev_event_type ev /= selectionNotify
          &&  not (ev_event_type ev == propertyNotify && ev_atom ev == selectionTarget && ev_propstate ev == 1))
      (go display' window evPtr)

  waitForEvents display' = do
    nbEvs <- pending display'
    when (nbEvs == 0) $ threadDelay _10ms >> waitForEvents display'

  _10ms = 10000



setClipboardSelection :: Selection -> IO ()
setClipboardSelection sel = void $ forkProcess $ do
        mapM_ hClose [stdin, stdout, stderr]
        setCurrentDirectory "/"
        bracket getXorgContext destroyXorgContext $ \ctx@XorgContext{..} -> do
          let clipboards = [defaultClipboard, primaryClipboard]
          mapM_ (\atom -> xSetSelectionOwner display atom ownWindow currentTime) clipboards
          advertiseSelection ctx sel
          return ()


selectionTypeToMime :: SelectionType -> ByteString
selectionTypeToMime (PNG _)    = "image/png"
selectionTypeToMime (JPEG _)   = "image/jpeg"
selectionTypeToMime (BITMAP _) = "image/bmp"
selectionTypeToMime (UTF8 _)   = "UTF8_STRING"

getContent :: SelectionType -> ByteString
getContent (PNG bytes)    = bytes
getContent (JPEG bytes)   = bytes
getContent (BITMAP bytes) = bytes
getContent (UTF8 txt)     = encodeUtf8 txt

advertiseSelection :: XorgContext -> Selection ->  IO ()
advertiseSelection ctx@XorgContext{..} sel = allocaXEvent (go [defaultClipboard, primaryClipboard])
  where
    go [] _ = return ()
    go clipboards evPtr = do
      nextEvent display evPtr
      ev <- getEvent evPtr
      case ev of
          SelectionRequest {..} -> do
              target' <- getAtomName display ev_target
              response <- case target' of
                Nothing -> return none
                Just atomName -> handleRequest ctx (selection sel) ev_requestor ev_property (toS atomName)

              sendSelectionNotify display ev_requestor ev_selection ev_target response ev_time
              go clipboards evPtr

          SelectionClear{..} -> go (filter (/= ev_selection) clipboards) evPtr

          _ -> go clipboards evPtr

handleRequest :: XorgContext -> SelectionType -> Window -> Atom -> Text -> IO Atom
handleRequest XorgContext{..} sel requestorWindow selection "TARGETS" = do
  targets <- internAtom display "TARGETS" True
  target <- internAtom display (StrConvOld.toS $ selectionTypeToMime sel) True
  changeProperty32 display requestorWindow selection aTOM propModeReplace [fromIntegral targets, fromIntegral target]
  return selection

handleRequest XorgContext{..} sel req prop targetStr =
  if targetStr == decodeUtf8 (selectionTypeToMime sel)
    then do
      target <- internAtom display (toS targetStr) True
      void $ withArrayLen (byteStringToCUChars $ getContent sel) $ \len bytes ->
          xChangeProperty display req prop target 8 propModeReplace bytes (fromIntegral len)
      return prop
    else return none

sendSelectionNotify :: Display -> Window -> Atom -> Atom -> Atom -> Time -> IO ()
sendSelectionNotify display req sel target prop time = allocaXEvent $ \ev -> do
  setEventType ev selectionNotify
  setSelectionNotify ev req sel target prop time
  sendEvent display req False 0 ev

byteStringToCUChars :: ByteString -> [CUChar]
byteStringToCUChars = map fromIntegral . unpack
