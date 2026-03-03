{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import CommandLine
import Control.Monad
import Data.Maybe
import Options.Applicative
import System.Directory
import System.Exit
import Utils

-- Main

main :: IO ()
main = do
  cmd <- execParser (info (commandParser <**> helper) fullDesc)
  case cmd of
    Verify opts -> verify opts
    Fuzz opts -> fuzz opts

verify :: VerifyOpts -> IO ()
verify opts =
  runErrorM (verifyProgram (verifyFile opts) (verifyMethod opts))
    >>= \case
      Left err -> putStrLn (red ("Error: \n") <> err) >> exitFailure
      Right res ->
        case verifyOutput opts of
          Just fileName -> writeFile fileName res
          Nothing -> putStrLn res

red :: String -> String
red str = "\ESC[31m" ++ str ++ "\ESC[0m"

fuzz :: FuzzOpts -> IO ()
fuzz opts =
  do
    dir <- makeAbsolute $ fuzzOutput opts
    createDirectoryIfMissing True dir
    setCurrentDirectory dir
    undefined

-- main :: IO ()
-- main = parseOptions =<< execParser optionsParser
--
-- parseOptions :: Commands -> IO ()
-- parseOptions cmds =
--   do
--     forM_ (verifyCMD cmds) $ \program ->
--       (verifyProgram program (methodCMD cmds) >>= printOut)
--     forM_ (xmlCMD cmds) $ \xmlFile ->
--       do
--         contents <- readFile xmlFile
--         res <- verifyXML contents
--         printOut res
--     putStrLn "Hello"
--
-- printOut :: Either String String -> IO ()
-- printOut (Left err) = putStrLn (red ("Error:\n") <> err) >> exitFailure
-- printOut (Right res) = putStrLn res >> exitSuccess
--
-- bold :: String -> String
-- bold str = "\ESC[1m" ++ str ++ "\ESC[0m"
--
-- green :: String -> String
-- green str = "\ESC[32m" ++ str ++ "\ESC[0m"
--
--
-- cyan :: String -> String
-- cyan str = "\ESC[96m" ++ str ++ "\ESC[0m"

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
-- print out
