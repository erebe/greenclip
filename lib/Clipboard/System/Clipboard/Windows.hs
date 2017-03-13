-- | System clipboard interface with unicode support.
--
-- For more information, see "Graphics.Win32.GDI.Clip"
-- or documentation for /GetClipboardData/ on MSDN.
module System.Clipboard.Windows
    ( setClipboardString
    , getClipboardString
    ) where

import System.Win32.Mem
    (globalAlloc, globalLock, globalUnlock, copyMemory, gHND)
import Graphics.Win32.GDI.Clip
    ( openClipboard, closeClipboard, emptyClipboard,
      getClipboardData, setClipboardData
    , cF_TEXT, ClipboardFormat
    , isClipboardFormatAvailable)
import Foreign.C
   (withCAString, peekCAString, withCWString, peekCWString)
import Foreign.Ptr
    (castPtr, nullPtr)
import Data.List
    (genericLength)
import Control.Exception
    (bracket_, bracket)
import Data.Maybe
    (isJust)

cF_UNICODETEXT :: ClipboardFormat
cF_UNICODETEXT = 13

setClipboardString :: String -> IO ()
setClipboardString str =
   withCWString str $ \cstring -> do
   mem <- globalAlloc gHND strLen
   bracket (globalLock mem) globalUnlock $ \mem' -> do
       copyMemory mem' (castPtr cstring) strLen
       bracket_ (openClipboard nullPtr) closeClipboard $ do
           emptyClipboard
           setClipboardData cF_UNICODETEXT mem'
           return ()
 where
   strLen = 2 * (genericLength str + 1)

getClipboardString :: IO (Maybe String)
getClipboardString =
   bracket_ (openClipboard nullPtr) closeClipboard $ do
     isUnicodeAvailable <- isClipboardFormatAvailable cF_UNICODETEXT
     if isUnicodeAvailable
       then do handle <- getClipboardData cF_UNICODETEXT
               mem <- globalLock handle
               str <- peekCWString (castPtr mem)
               globalUnlock mem
               return $ Just str
       else do isAnsiAvailable <- isClipboardFormatAvailable cF_TEXT
               if isAnsiAvailable
                 then do handle <- getClipboardData cF_TEXT
                         mem <- globalLock handle
                         str <- peekCAString (castPtr mem)
                         globalUnlock mem
                         return $ Just str
                 else return Nothing
