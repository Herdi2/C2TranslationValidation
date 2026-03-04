{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import CommandLine
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Options.Applicative
import System.Directory
import System.Exit
import System.Random
import Utils

-- Main

main :: IO ()
main = do
  cmd <- execParser (info (commandParser <**> helper) fullDesc)
  case cmd of
    Verify opts -> verify opts
    Fuzz opts -> fuzz opts
    Campaign opts -> campaign opts

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
    seed <- fromMaybe randomIO (return <$> (fuzzSeed opts))
    let className = "Klass" <> show seed
    runErrorM (fuzzProgram seed className)
      >>= \case
        Left err -> putStrLn (red ("Error: \n") <> err) >> exitFailure
        Right prog ->
          case (fuzzDir opts) of
            Nothing -> putStrLn prog
            Just file -> makeAbsolute file >>= \dir -> writeFile (dir <> className <> ".java") prog

campaign :: CampaignOpts -> IO ()
campaign opts =
  do
    dir <- makeAbsolute $ campaignDir opts
    createDirectoryIfMissing True dir
    setCurrentDirectory dir
    replicateM_ (campaignNum opts) $
      do
        seed <- randomIO
        let className = "Klass" <> show seed
        let resFile = dir <> className <> ".res"
        res <-
          runErrorM
            ( do
                liftIO $ putStrLn $ "Generating Java file with seed: " <> show seed
                prog <- fuzzProgram seed className
                let javafile = dir <> className <> ".java"
                liftIO $ writeFile javafile prog
                -- Method name "method" hardcoded by fuzzer, ugly
                liftIO $ putStrLn $ "Compiling file " <> className <> ".java"
                xmlContent <- compileJavaProgram javafile "method"
                liftIO $ putStrLn $ "Verifying file " <> className <> ".java"
                verifyXML xmlContent
            )
        case res of
          Left err ->
            putStrLn $ "Error: " <> err
          Right satRes ->
            putStrLn ("Writing results to " <> resFile)
              >> writeFile resFile satRes

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
