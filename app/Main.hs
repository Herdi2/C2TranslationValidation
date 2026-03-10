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
            putStrLn ("Results: " <> satRes)
              >> putStrLn ("Writing results to " <> resFile)
              >> writeFile resFile satRes
