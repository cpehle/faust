{-# LANGUAGE OverloadedStrings #-}
module Main where
       
import Data.Monoid (mempty)
import Linenoise (linenoise, linenoiseHistoryAdd)
import Parser (parseTerm)
import Text.Trifecta (parseByteString)

readEvalPrintLoop :: IO ()
readEvalPrintLoop
  = do maybeLine <- linenoise ">>> "
       case maybeLine of
         Nothing   -> return ()
         Just line -> do
           print line
           linenoiseHistoryAdd line
           let res = parseByteString parseTerm mempty line
           print res                 
           readEvalPrintLoop

main :: IO ()
main = readEvalPrintLoop




































