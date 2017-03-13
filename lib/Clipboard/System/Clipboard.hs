
{-# LANGUAGE CPP #-}

-- | System Clipboard Interface. It should work on both Windows and Unix (X11).
--   The latter is still experimental.
module System.Clipboard
    ( -- * Clipboard interface
      setClipboardString
    , getClipboardString
    , modifyClipboardString
    ) where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.Clipboard.Windows as OS
#else
import qualified System.Clipboard.X11 as OS
#endif

-- | Writes a string to the clipboard.
setClipboardString :: String -> IO ()
setClipboardString = OS.setClipboardString

-- | Gets the contents of the clipboard as a 'String'.
--   Returns 'Nothing' if the clipboard doesn't contain /textual/ data.
getClipboardString :: IO (Maybe String)
getClipboardString = OS.getClipboardString

-- | Modifies the clipboard content.
--   If the clipboard has /textual/ data, this function modifies its content
--   and return 'True'. Otherwise, it does nothing and return 'False'.
modifyClipboardString :: (String -> String) -> IO Bool
modifyClipboardString f = do
 s <- getClipboardString
 case s of
   Nothing -> return False
   Just sc -> setClipboardString (f sc) >> return True
