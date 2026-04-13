{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import CommandLine
import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe
import Data.SBV
import Data.SBV.Control
import Data.Time
import Data.Time.Clock
import Options.Applicative
import System.Clock
import System.Directory
import System.Exit
import System.FilePath
import System.Random
import Utils
import Verifier.ErrorHandler

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
    -- Within the chosen directory, we create a directory
    -- with current date and time which will hold the results
    dir <- makeAbsolute $ campaignDir opts
    currDate <- getDate
    let outputDir = dir </> currDate
    createDirectoryIfMissing True outputDir
    setCurrentDirectory outputDir

    -- The log file will store all information about the campaign
    -- run in the following CSV headers:
    -- Time-stamp, seed, SMT result/error type, error message, SMT time taken, total time taken
    let csvHeaders = "time-stamp,seed,smt-result,error-message,smt-time,total-time\n"
        logfile = "campaign.csv"
    appendFile logfile csvHeaders
    -- testCount is used to name the files.
    -- Each file will get the name Test{testCount}.java
    forM_ [1 .. campaignNum opts] $ \testCount ->
      do
        seed <- randomIO -- Fuzzer seed is randomly chosen
        -- We measure two times
        -- 1. Time taken to solve the SMT formula, given by SBV (Z3)
        -- 2. Total time taken to construct and solve SMT formula
        smtTiming <- newIORef 0
        totalTiming <- newIORef 0
        let className = "Test" <> show testCount
            resFile = className <> ".smt"
            javafile = className <> ".java"
            campaignConfig = mkConfig (Just resFile) (campaignZ3Timeout opts) smtTiming
        currTime <- getCurrentTime
        satRes <-
          runErrorM
            ( do
                prog <- fuzzProgram seed className
                liftIO $ writeFile javafile prog
                -- NOTE: Fuzzer always generates methods with name "method"
                start <- liftIO $ getTime Monotonic
                xmlContent <- compileJavaProgram javaBin javafile "method"
                res <- verifyXML campaignConfig xmlContent
                end <- liftIO $ getTime Monotonic
                liftIO $ writeIORef totalTiming (fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1e9)
                return res
            )
        -- Output written in a format that is easy to parse 14725905904395840929
        -- [currTime] <seed> Error <err message>
        -- [currTime] <seed> Unsat <time taken>
        -- [currTime] <seed> Sat <model> <time taken>
        smtTime <- readIORef smtTiming
        totalTime <- readIORef totalTiming
        let prefix = "[" <> show currTime <> "] " <> javafile <> " "
        let (smtResult, errorMessage) =
              case satRes of
                Left err -> (show (_errorType err), show (_errorMessage err))
                Right (SatResult res) ->
                  case res of
                    (Satisfiable model _) ->
                      ("Sat", show model)
                    (Unsatisfiable {}) ->
                      ("Unsat", "")
                    (Unknown _ reason) ->
                      ("Unknown", show reason)
                    _ -> ("Error", "Unsupported SMT result")
            logOutput = intercalate "," [show currTime, show seed, smtResult, errorMessage, show smtTime, show totalTime]
        -- deleteIfExists javafile
        appendFile logfile (logOutput <> "\n")
        deleteIfExists resFile
        deleteIfExists (className <> ".xml")
        putStrLn $ prefix <> intercalate " " [smtResult, errorMessage, show smtTime, show totalTime]

deleteIfExists :: FilePath -> IO ()
deleteIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

getDate :: IO String
getDate =
  do
    now <- getCurrentTime
    tz <- getCurrentTimeZone
    let local = utcToLocalTime tz now
    return $ formatTime defaultTimeLocale "%Y-%m-%d#%H:%M:%S" local
