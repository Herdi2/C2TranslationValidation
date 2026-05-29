{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import CommandLine
import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.IO.Class
import Data.Either (fromRight)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe
import Data.SBV
import Data.SBV.Control
import Data.Time
import Data.Time.Clock
import Debug.Trace
import Effectful.Error.Static
import Options.Applicative
import Prettyprinter (pretty)
import System.Clock
import System.Directory
import System.Exit
import System.FilePath
import System.Random
import Utils
import Verifier.ErrorHandler
import Verifier.Graph
import Verifier.Verify

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
  case (globalCommand globalOpts) of
    Verify opts -> verify globalOpts opts
    Compare opts -> compareGraphs opts
    Fuzz opts -> fuzz opts
    Campaign opts -> campaign globalOpts opts
    AST opts -> ast globalOpts opts

ast :: GlobalOpts -> AstOpts -> IO ()
ast globalOpts opts =
  do
    let javaBin = globalJavaBin globalOpts
    javaFile <- makeAbsolute $ astFile opts
    res <-
      runErrorM $
        do
          xmlContent <-
            compileJavaProgram
              javaBin
              javaFile
              (astMethod opts)
              True
              (globalCtrlBugs globalOpts)
              (globalMemBugs globalOpts)
              (globalWuBugs globalOpts)
          parseGraphs xmlContent
    case res of
      Left err -> print err
      Right (before, after) ->
        do
          print (pretty before)
          print (pretty after)
          putStrLn $ "Node count before: " <> show (nodeTypesCount before)
          putStrLn $ "Node count after: " <> show (nodeTypesCount before)

verify :: GlobalOpts -> VerifyOpts -> IO ()
verify globalOpts opts =
  do
    -- Within the chosen directory, we create a directory
    -- with current date and time which will hold the results
    let javaBin = (globalJavaBin globalOpts)
    javaInput <- makeAbsolute $ verifyPath opts
    isDir <- doesDirectoryExist javaInput
    isFile <- doesFileExist javaInput
    dir <- makeAbsolute $ verifyOutput opts
    currDate <- getDate
    let outputDir = dir </> ("Verify#" <> currDate)
    createDirectoryIfMissing True outputDir
    setCurrentDirectory outputDir

    -- The log file will store all information about the campaign
    -- run in the following CSV headers:
    -- Time-stamp, seed, SMT result/error type, error message, SMT time taken, total time taken
    let logfile = "verify.csv"
    appendFile logfile csvHeaders
    -- testCount is used to name the files.
    -- Each file will get the name Test{testCount}.java
    when (isFile) $
      replicateM_ (verifyIteration opts) $
        do
          logInfo <-
            verifyProgram
              javaInput
              (verifyMethod opts)
              javaBin
              0
              (60 * 1000)
              True
              True
              (globalCtrlBugs globalOpts)
              (globalMemBugs globalOpts)
              (globalWuBugs globalOpts)
          appendFile logfile (show logInfo <> "\n")
    when (isDir) $
      do
        -- We wish to verify all java files in a directory
        entries <- listDirectory javaInput
        let javaFiles = [javaInput </> file | file <- entries, takeExtension file == ".java"]
        forM_ javaFiles $ \javaFile ->
          replicateM_ (verifyIteration opts) $
            do
              logInfo <-
                verifyProgram
                  javaFile
                  (verifyMethod opts)
                  javaBin
                  0
                  (60 * 1000)
                  True
                  True
                  (globalCtrlBugs globalOpts)
                  (globalMemBugs globalOpts)
                  (globalWuBugs globalOpts)
              appendFile logfile (show logInfo <> "\n")
    when (not isDir && not isFile) $ putStrLn $ (red "ERROR") <> " : " <> javaInput <> " is not a file or directory"

red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

compareGraphs :: CompareOpts -> IO ()
compareGraphs opts =
  do
    xmlFile <- makeAbsolute (compareXML opts)
    xmlContent <- readFile xmlFile
    replicateM_ (compareIteration opts) $
      do
        currTime <- getCurrentTime
        smtTiming <- newIORef 0
        totalTiming <- newIORef 0
        let smtConfig = mkConfig Nothing (60 * 1000) smtTiming
        satRes <-
          runErrorM
            ( do
                start <- liftIO $ getTime Monotonic
                res <- verifyXML smtConfig xmlContent
                end <- liftIO $ getTime Monotonic
                liftIO $ writeIORef totalTiming (fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1e9)
                return res
            )
        let (smtResult, errorMessage) =
              case satRes of
                Left err -> (show (_errorType err), show (_errorMessage err))
                Right (SatResult res) ->
                  case res of
                    (Satisfiable _ model) ->
                      ("Sat", show satRes)
                    (Unsatisfiable {}) ->
                      ("Unsat", "")
                    (Unknown _ reason) ->
                      ("Unknown", show reason)
                    _ -> ("Error", "Unsupported SMT result")
        smtTime <- readIORef smtTiming
        totalTime <- readIORef totalTiming
        putStrLn $
          "["
            <> show currTime
            <> "] "
            <> intercalate " " [xmlFile, show smtTime, show totalTime, smtResult, errorMessage]

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

campaign :: GlobalOpts -> CampaignOpts -> IO ()
campaign globalOpts opts =
  do
    -- Within the chosen directory, we create a directory
    -- with current date and time which will hold the results
    let javaBin = globalJavaBin globalOpts
    dir <- makeAbsolute $ campaignDir opts
    currDate <- getDate
    let outputDir = dir </> ("Campaign" <> currDate)
    createDirectoryIfMissing True outputDir
    setCurrentDirectory outputDir

    -- The log file will store all information about the campaign
    -- run in the following CSV headers:
    -- Time-stamp, seed, SMT result/error type, error message, SMT time taken, total time taken
    let logfile = "campaign.csv"
    appendFile logfile csvHeaders
    -- testCount is used to name the files.
    -- Each file will get the name Test{testCount}.java
    forM_ [1 .. campaignNum opts] $ \testCount ->
      do
        seed <- randomIO -- Fuzzer seed is randomly chosen
        -- We measure two times
        -- 1. Time taken to solve the SMT formula, given by SBV (Z3)
        -- 2. Total time taken to construct and solve SMT formula
        let className = "Test" <> show testCount
            javafile = className <> ".java"
        fuzzRes <-
          runErrorM
            ( do
                prog <- fuzzProgram seed className
                liftIO $ writeFile javafile prog
            )
        case fuzzRes of
          Left err -> putStrLn $ show err
          Right _ ->
            do
              logInfo <-
                verifyProgram
                  javafile
                  "method"
                  javaBin
                  seed
                  (campaignZ3Timeout opts)
                  True
                  True
                  (globalCtrlBugs globalOpts)
                  (globalMemBugs globalOpts)
                  (globalWuBugs globalOpts)
              appendFile logfile (show logInfo <> "\n")

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

data LogInfo
  = LogInfo
  { _timestamp :: UTCTime,
    _javaFile :: FilePath,
    _seed :: Word64,
    _smtResult :: String,
    _errMessage :: String,
    _smtTime :: NominalDiffTime,
    _totalTime :: NominalDiffTime,
    _dataNodeCount :: Integer,
    _ctrlNodeCount :: Integer,
    _memNodeCount :: Integer
  }

instance Show LogInfo where
  show (LogInfo timeStamp javaFile seed smtResult errMessage smtTime totalTime d c m) =
    intercalate "," [show timeStamp, javaFile, show seed, smtResult, errMessage, show smtTime, show totalTime, show d, show c, show m]

csvHeaders :: String
csvHeaders = "time-stamp,java-file,seed,smt-result,error-message,smt-time,total-time,data-nodes,ctrl-nodes,mem-nodes\n"

-- Runs the verification and runs results on a given java program
-- Everything is done in the current directory.
verifyProgram ::
  -- | Java file
  FilePath ->
  -- | Java method
  String ->
  -- | Java binary
  FilePath ->
  -- | Seed (-1 if no seed used)
  Word64 ->
  -- | SMT solver timeout
  Integer ->
  -- | Delete the generated files?
  Bool ->
  -- | Print information to stdout?
  Bool ->
  -- | Control bugs
  Int ->
  -- | Memory bugs
  Int ->
  -- | Reintroduce Wu's bugs
  Bool ->
  IO LogInfo
verifyProgram javaFile javaMethod javaBin seed z3Timeout deleteFiles printInfo ctrlBugs memBugs reintroduce =
  do
    smtTiming <- newIORef 0
    totalTiming <- newIORef 0
    nodeCounts <- newIORef (0, 0, 0)
    let resFile = javaFile <> ".smt"
        campaignConfig = mkConfig (Just resFile) z3Timeout smtTiming
    currTime <- getCurrentTime
    satRes <-
      runErrorM
        ( do
            xmlContent <- compileJavaProgram javaBin javaFile javaMethod deleteFiles ctrlBugs memBugs reintroduce
            start <- liftIO $ getTime Monotonic
            (before, after) <- parseGraphs xmlContent
            verification <- liftIO $ runVerification campaignConfig before after
            end <- liftIO $ getTime Monotonic
            liftIO $ writeIORef totalTiming (fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1e9)
            let (d1, c1, m1) = nodeTypesCount before
                (d2, c2, m2) = nodeTypesCount after
            liftIO $ writeIORef nodeCounts (d1 + d2, c1 + c2, m1 + m2)
            case verification of
              Left (VerifyException err) -> throwError err
              Right res -> return res
        )
    smtTime <- readIORef smtTiming
    totalTime <- readIORef totalTiming
    let prefix = "[" <> show currTime <> "] " <> javaFile <> " "
        (smtResult, errorMessage) =
          case satRes of
            Left err -> (show (_errorType err), show (_errorMessage err))
            Right (SatResult res) ->
              case res of
                (Satisfiable _ model) ->
                  ("Sat", show (show satRes))
                (Unsatisfiable {}) ->
                  ("Unsat", "")
                (Unknown _ reason) ->
                  ("Unknown", show reason)
                _ -> ("Error", "Unsupported SMT result")
    putStrLn $ resFile
    when deleteFiles $ deleteIfExists resFile
    when printInfo $
      putStrLn $
        prefix <> intercalate " " [show seed, smtResult, errorMessage, show smtTime, show totalTime]
    (dataNodes, ctrlNodes, memNodes) <- readIORef nodeCounts
    return $
      LogInfo
        currTime
        javaFile
        seed
        smtResult
        errorMessage
        smtTime
        totalTime
        dataNodes
        ctrlNodes
        memNodes
