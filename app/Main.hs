{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import CommandLine
import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Data.SBV
import Data.SBV.Control
import Data.Time.Clock
import Options.Applicative
import System.Directory
import System.Exit
import System.Random
import Utils

-- Main

-- | Produces an SMT config with:
-- 1. FilePath to output SMT formula (if wanted)
-- 2. Timeout
-- 3. Timing
mkConfig :: Maybe FilePath -> Integer -> IORef NominalDiffTime -> SMTConfig
mkConfig smtFile tmout timingRef =
  z3
    { transcript = smtFile,
      solverSetOptions = [SetTimeOut tmout],
      timing = SaveTiming timingRef,
      printBase = 10
    }

main :: IO ()
main = do
  globalOpts <- execParser (info (globalParser <**> helper) fullDesc)
  let javaBin = globalJavaBin globalOpts
  case (globalCommand globalOpts) of
    Verify opts -> verify javaBin opts
    Fuzz opts -> fuzz opts
    Campaign opts -> campaign javaBin opts

verify :: FilePath -> VerifyOpts -> IO ()
verify javaBin opts =
  do
    timingRef <- liftIO $ newIORef 0
    let smtfile = verifyOutput opts
    runErrorM
      ( do
          let fuzzConfig = mkConfig smtfile (60 * 1000) timingRef
          liftIO $ putStrLn $ "Compiling file " <> (verifyFile opts)
          xmlContent <- compileJavaProgram javaBin (verifyFile opts) (verifyMethod opts)
          liftIO $ putStrLn $ "Verifying file " <> (verifyFile opts)
          verifyXML fuzzConfig xmlContent
      )
      >>= \case
        Left err -> putStrLn (show err) >> exitFailure
        Right (SatResult satRes) ->
          do
            t <- readIORef timingRef
            putStrLn ("Verification done: " <> show t)
            when (isJust smtfile) $ putStrLn ("SMT written to: " <> (fromJust smtfile))
            case satRes of
              (Unsatisfiable {}) -> putStrLn "No bug found"
              (Satisfiable {}) ->
                do
                  putStrLn "Found bug with assignment:"
                  print $ getModelDictionary satRes
              _ -> putStrLn (red $ "Unknown result") >> exitFailure

red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

fuzz :: FuzzOpts -> IO ()
fuzz opts =
  do
    seed <- fromMaybe randomIO (return <$> (fuzzSeed opts))
    let className = "Klass" <> show seed
    runErrorM (fuzzProgram seed className)
      >>= \case
        Left err -> putStrLn (red ("Error: \n") <> show err) >> exitFailure
        Right prog ->
          case (fuzzDir opts) of
            Nothing -> putStrLn prog
            Just file -> makeAbsolute file >>= \dir -> writeFile (dir <> className <> ".java") prog

campaign :: FilePath -> CampaignOpts -> IO ()
campaign javaBin opts =
  do
    dir <- makeAbsolute $ campaignDir opts
    createDirectoryIfMissing True dir
    setCurrentDirectory dir
    let logfile = "campaign.log"
    replicateM_ (campaignNum opts) $
      do
        seed <- randomIO
        timingRef <- newIORef 0
        let className = "Klass" <> show seed
            resFile = dir <> className <> ".smt"
            javafile = dir <> className <> ".java"
            campaignConfig = mkConfig (Just resFile) (campaignZ3Timeout opts) timingRef
        satRes <-
          runErrorM
            ( do
                -- liftIO $ putStrLn $ "Generating Java file with seed: " <> show seed
                prog <- fuzzProgram seed className
                liftIO $ writeFile javafile prog
                -- Method name "method" hardcoded by fuzzer, ugly
                -- liftIO $ putStrLn $ "Compiling file " <> className <> ".java"
                xmlContent <- compileJavaProgram javaBin javafile "method"
                -- liftIO $ putStrLn $ "Verifying file " <> className <> ".java"
                verifyXML campaignConfig xmlContent
                -- liftIO $ putStrLn $ "SMT written to " <> resFile
            )
        -- Output written in a format that is easy to parse
        -- [currTime] <seed> Error <err message>
        -- [currTime] <seed> Unsat <time taken>
        -- [currTime] <seed> Sat <model> <time taken>
        currTime <- getCurrentTime
        timetaken <- readIORef timingRef
        let prefix = "[" <> show currTime <> "] " <> show seed <> " "
        let logoutput =
              case satRes of
                Left err -> prefix <> show err
                Right (SatResult res) ->
                  case res of
                    (Satisfiable {}) ->
                      prefix <> "Sat " <> show (M.toList $ getModelDictionary res) <> " " <> show timetaken
                    (Unsatisfiable {}) ->
                      prefix <> "Unsat " <> show timetaken
                    (Unknown _ reason) ->
                      prefix <> "Error " <> show timetaken <> " " <> show reason
                    _ -> prefix <> "Error, unsupported SMTResult"
        appendFile logfile (logoutput <> "\n")
        deleteIfExists javafile
        deleteIfExists resFile
        deleteIfExists (className <> ".xml")
        putStrLn logoutput

deleteIfExists :: FilePath -> IO ()
deleteIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path
