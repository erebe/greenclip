
{-# LANGUAGE CPP #-}

-- | System Clipboard Interface. It should work on both Windows and Unix (X11).
--   The latter is still experimental.
module System.Clipboard
    ( -- * Clipboard interface
      setClipboardString
    , getClipboardString
    , getPrimaryClipboard
    , modifyClipboardString
    ) where

import qualified System.Clipboard.X11 as OS

-- | Writes a string to the clipboard.
setClipboardString :: String -> IO ()
setClipboardString = OS.setClipboardString

-- | Gets the contents of the clipboard as a 'String'.
--   Returns 'Nothing' if the clipboard doesn't contain /textual/ data.
getClipboardString :: IO (Maybe String)
getClipboardString = OS.getClipboardString

getPrimaryClipboard :: IO (Maybe String)
getPrimaryClipboard = OS.getPrimaryClipboard

-- | Modifies the clipboard content.
--   If the clipboard has /textual/ data, this function modifies its content
--   and return 'True'. Otherwise, it does nothing and return 'False'.
modifyClipboardString :: (String -> String) -> IO Bool
modifyClipboardString f = do
 s <- getClipboardString
 case s of
   Nothing -> return False
   Just sc -> setClipboardString (f sc) >> return True
