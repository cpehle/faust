{-# LANGUAGE OverloadedStrings #-}
module Main where
       
import Data.Monoid (mempty)
import System.Console.Readline
import Parser (parseTerm)
import Text.Trifecta (parseString)

readEvalPrintLoop :: IO ()
readEvalPrintLoop
  = do maybeLine <- readline ">>> "
       case maybeLine of
         Nothing   -> return ()
         Just line -> do
           print line
           addHistory line
           let res = parseString parseTerm mempty line
           print res                 
           readEvalPrintLoop

main :: IO ()
main = readEvalPrintLoop




































