
{-# LANGUAGE RecordWildCards #-}

module System.Clipboard.X11
  ( getClipboardString
  , setClipboardString
  ) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Posix.Process     (forkProcess)

import Codec.Binary.UTF8.String (decode, encode)
import Control.Monad
import Data.Maybe
import Foreign                  (peekByteOff)
import Foreign.C.Types          (CChar, CUChar)
import Foreign.Marshal.Array    (withArrayLen)
import System.Directory         (setCurrentDirectory)
import System.IO                (hClose, stderr, stdin, stdout)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import System.Timeout     (timeout)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

getClipboardString :: IO (Maybe String)
getClipboardString = do
    (display, window, clipboards) <- readIORef initialSetup
    inp <- internAtom display "clipboard_get" False
    target <- internAtom display "UTF8_STRING" True
    holder <- newEmptyMVar :: IO (MVar String)
    _ <- forkIO $ swpanEventLoop display window inp holder
    xConvertSelection display (head clipboards) target inp window currentTime
    Just <$> takeMVar holder
  where
    swpanEventLoop d w i h = const () <$> timeout (2 * 1000000) (clipboardInputWait d w i h)

clipboardInputWait :: Display -> Window -> Atom -> MVar String -> IO ()
clipboardInputWait display' window' inp' holder' = allocaXEvent (go display' window' inp' holder')
  where
  go display window inp holder evPtr = do
    nextEvent display evPtr
    ev <- getEvent evPtr
    if ev_event_type ev == selectionNotify
       then do
         selection <- charsToString . fromMaybe mempty <$> getWindowProperty8 display inp window
         putMVar holder selection
       else go display window inp holder evPtr

charsToString :: [CChar] -> String
charsToString = decode . map fromIntegral

setClipboardString :: String -> IO ()
setClipboardString str = void $ forkProcess $ do
        mapM_ hClose [stdin, stdout, stderr]
        setCurrentDirectory "/"
        (display, window, clipboards) <- readIORef initialSetup
        mapM_ (\atom -> xSetSelectionOwner display atom window currentTime) clipboards
        advertiseSelection display clipboards (stringToChars str)
        cleanup display window

advertiseSelection :: Display -> [Atom] -> [CUChar] -> IO ()
advertiseSelection display clipboards' str = allocaXEvent (go clipboards')
  where
    go [] _ = return ()
    go clipboards evPtr = do
      nextEvent display evPtr
      ev <- getEvent evPtr
      case ev of
          SelectionRequest {..} -> do
              target' <- getAtomName display ev_target
              res <- handleOutput display ev_requestor ev_property target' str
              sendSelectionNotify display ev_requestor ev_selection ev_target res ev_time
              go clipboards evPtr

          _ | ev_event_type ev == selectionClear -> do
              target <- peekByteOff evPtr 40 :: IO Atom
              go (filter (/= target) clipboards) evPtr

          _ -> go clipboards evPtr

handleOutput :: Display -> Window -> Atom -> Maybe String -> [CUChar] -> IO Atom
handleOutput display req prop (Just "UTF8_STRING") str = do
    prop' <- getAtomName display prop
    if isNothing prop' then handleOutput display req prop Nothing str else do
        target <- internAtom display "UTF8_STRING" True
        void $ withArrayLen str $ \len str' ->
            xChangeProperty display req prop target 8 propModeReplace str'
                            (fromIntegral len)
        return prop
handleOutput _ _ _ _ _ = return none

sendSelectionNotify :: Display -> Window -> Atom -> Atom -> Atom -> Time -> IO ()
sendSelectionNotify display req sel target prop time = allocaXEvent $ \ev -> do
    setEventType ev selectionNotify
    setSelectionNotify ev req sel target prop time
    sendEvent display req False 0 ev

stringToChars :: String -> [CUChar]
stringToChars = map fromIntegral . encode

initialSetup :: IORef (Display, Window, [Atom])
initialSetup = unsafePerformIO $ do
    display <- openDisplay ""
    window <- createSimpleWindow display (defaultRootWindow display)
                                 0 0 1 1 0 0 0
    clipboards <- internAtom display "CLIPBOARD" False
    newIORef (display, window, [clipboards, pRIMARY])
{-# NOINLINE initialSetup #-}

cleanup :: Display -> Window -> IO ()
cleanup display window = do
    destroyWindow display window
    closeDisplay display
