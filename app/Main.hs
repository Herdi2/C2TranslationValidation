{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import CommandLine
import Control.Monad
import Data.Maybe
import Options.Applicative
import Utils

main :: IO ()
main = parseOptions =<< execParser optionsParser

parseOptions :: Commands -> IO ()
parseOptions cmds =
  do
    forM_ (verify cmds) $ \program ->
      (verifyProgram program (methodName cmds) >>= printOut)

printOut :: Either String String -> IO ()
printOut (Left err) = putStrLn $ red ("Error: ") <> err
printOut (Right res) = putStrLn res

bold :: String -> String
bold str = "\ESC[1m" ++ str ++ "\ESC[0m"

green :: String -> String
green str = "\ESC[32m" ++ str ++ "\ESC[0m"

red :: String -> String
red str = "\ESC[31m" ++ str ++ "\ESC[0m"

cyan :: String -> String
cyan str = "\ESC[96m" ++ str ++ "\ESC[0m"

-- do
--   let outPath = "/home/herdi/Desktop/master-work/C2TranslationValidation/output/"
--   fuzzProgram outPath
--     >>= \case
--       Left err -> putStrLn err
--       Right javaFile ->
--         compileJavaProgram javaFile "method" outPath
--           >>= \case
--             Left err -> putStrLn err
--             Right xml ->
--               verifyXML (outPath ++ xml)
--                 >>= \case
--                   Left err -> putStrLn err
--                   Right res ->
--                     do
--                       print res
--                       print javaFile
--
-- out <- compareOutput javaFile "method"
-- print out
