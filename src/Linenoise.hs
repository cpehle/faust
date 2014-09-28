{-# LANGUAGE ForeignFunctionInterface #-}

module Linenoise where

import Foreign
import Foreign.C.Types(CInt(..), CChar)
import Data.ByteString

linenoise :: ByteString -> IO (Maybe ByteString)
linenoise prompt = do
          ptr <- useAsCString prompt linenoiseC
          line <- maybePeek packCString ptr
          free ptr
          return line
foreign import ccall "linenoise.h linenoise" linenoiseC :: Ptr CChar -> IO (Ptr CChar)

linenoiseHistoryAdd :: ByteString -> IO ()
linenoiseHistoryAdd line = do _ <- useAsCString line linenoiseHistoryAddC
                              return ()
foreign import ccall "linenoise.h linenoiseHistoryAdd" linenoiseHistoryAddC :: Ptr CChar -> IO CInt

linenoiseHistorySetMaxLen :: Int -> IO ()
linenoiseHistorySetMaxLen len = do _ <- linenoiseHistorySetMaxLenC $ fromIntegral len
                                   return ()
foreign import ccall "linenoise.h linenoiseHistorySetMaxLen" linenoiseHistorySetMaxLenC :: CInt -> IO CInt

linenoiseClearScreen :: IO ()
linenoiseClearScreen = linenoiseClearScreenC
foreign import ccall "linenoise.h linenoiseClearScreen" linenoiseClearScreenC :: IO ()

linenoiseSetMultiLine :: Bool -> IO ()
linenoiseSetMultiLine b = linenoiseSetMultiLineC $ fromBool b
foreign import ccall "linenoise.h linenoiseSetMultiLine" linenoiseSetMultiLineC :: CInt -> IO ()

linenoisePrintKeyCodes :: IO ()
linenoisePrintKeyCodes = linenoisePrintKeyCodesC
foreign import ccall "linenoise.h linenoisePrintKeyCodes" linenoisePrintKeyCodesC :: IO ()

