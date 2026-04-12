{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Utils where

import Control.Exception
import Data.List (intercalate, unsnoc)
import Data.SBV
import Debug.Trace
import Effectful
import Effectful.Error.Static
import Fuzzer.Gen
import Prettyprinter (pretty)
import System.Directory (getCurrentDirectory)
import System.Exit
import System.Process
import Verifier.ErrorHandler
import Verifier.Graph
import Verifier.GraphBuilder
import Verifier.GraphParser
import Verifier.Verify

type ErrorM = Eff '[Error CustomError, IOE]

runErrorM :: ErrorM a -> IO (Either CustomError a)
runErrorM = runEff . runErrorNoCallStack

-- | Given a Java file and a method within, return the output of the compiler and the interpreter
-- NOTE: We assume the class who's method we'll compile shared the name with the Java file
compareOutput :: String -> String -> IO (Either String (String, String))
compareOutput javaFile methodName =
  case extractClassName javaFile of
    Nothing -> return $ Left $ "Invalid Java file " <> javaFile
    Just (_path, javaClass) ->
      do
        let compileCommands =
              [ "-Xbatch", -- Makes sure compilation finishes
                "-XX:-TieredCompilation", -- C2 only
                "-XX:CompileCommand=compileonly," ++ javaClass ++ "::" ++ methodName, -- Compile only `javaClass::method`
                "-XX:-UseCompressedOops",
                -- Print floating-point numbers as binaries
                "-XX:+PrintFloatBits",
                javaFile
              ]
            interpreterCmds =
              [ "-Xint",
                javaFile
              ]
        (compExitCode, compRes, compErr) <-
          readCreateProcessWithExitCode
            ( (proc "java" compileCommands)
                { std_out = CreatePipe
                }
            )
            ""
        (intExitCode, intRes, intErr) <-
          readCreateProcessWithExitCode
            ( (proc "java" interpreterCmds)
                { std_out = CreatePipe
                }
            )
            ""
        case (compExitCode, intExitCode) of
          (ExitSuccess, ExitSuccess) ->
            return $ Right $ (compRes, intRes)
          _ ->
            return $
              Left $
                unlines
                  [ "Compiler exited with code " ++ show compExitCode ++ ": " ++ compErr,
                    "Interpreter exited with code " ++ show intExitCode ++ ": " ++ intErr
                  ]

-- | Given XML content representing the C2 IR, it parses the `graphName`
-- graph into an internal graph representation.
createGraph :: String -> String -> Either CustomError Graph
createGraph xmlContent graphName =
  do
    parsedGraph <- parseGraph graphName xmlContent
    builtGraph <- buildGraph parsedGraph
    return builtGraph

-- | Given XML content representing the C2 IR, parses the "After Parsing" and "Before Matching"
-- graphs into internal graph representations.
parseGraphs :: String -> Either CustomError (Graph, Graph)
parseGraphs xmlContent =
  do
    beforeGraph <- parseGraph "After Parsing" xmlContent
    afterGraph <- parseGraph "Before Matching" xmlContent
    before <- buildGraph beforeGraph
    after <- buildGraph afterGraph
    return (before, after)

-- | Verifies the given XML file
verifyXML :: SMTConfig -> String -> ErrorM SatResult
verifyXML smtConfig xmlContent =
  case (parseGraphs xmlContent) of
    Left err -> throwError err
    Right (before, after) | before == after -> throwError $ verificationError "The graphs are equal!"
    Right (before, after) ->
      do
        result <- (liftIO $ runVerification smtConfig before after)
        case result of
          Left (VerifyException err) -> throwError err
          Right res -> return res

-- | Given a path to a Java file, e.g. /hello/this/path/Klass.java
-- returns the Java class name and path: (/hello/this/path, Klass)
extractClassName :: String -> Maybe (String, String)
extractClassName fullPath =
  case unsnoc (splitPath fullPath) of
    Nothing -> Nothing
    Just (path, fileName) ->
      case span ((/=) '.') fileName of
        (className, ending) | ending == ".java" -> Just (intercalate "/" path, className)
        _ -> Nothing
  where
    splitPath path = reverse <$> splitPath' path ""
    splitPath' [] acc = [acc]
    splitPath' ('/' : rest) acc = acc : splitPath' rest ""
    splitPath' (x : xs) acc = splitPath' xs (x : acc)

-- | Given a Java program, the method within to be compiled, and an output path
-- NOTE: Right now -Xcomp is not used to trigger speculative optimizations.
-- However, that might change (possible add cmd flag)
compileJavaProgram :: FilePath -> String -> String -> ErrorM String
compileJavaProgram javaBin javaFile methodName =
  case extractClassName javaFile of
    Nothing -> throwError $ parseError $ "Invalid Java file " <> javaFile
    Just (_path, javaClass) ->
      do
        let compileCommands =
              [ -- Make sure compilation finishes before execution
                "-Xbatch", -- Makes sure compilation finishes
                -- Compile with C2 only
                "-XX:-TieredCompilation", -- C2 only
                -- Compile only `javaClass::method`
                "-XX:CompileCommand=compileonly," ++ javaClass ++ "::" ++ methodName,
                -- Do not compress pointers, to simplify object and array handling
                "-XX:-UseCompressedOops",
                -- Print floating-point numbers as binaries (NOTE: Custom JDK flag)
                "-XX:+PrintFloatBits",
                -- Delay arithmetic optimizations to after parsing (NOTE: Custom JDK flag)
                "-XX:+DelayArithmeticOpts",
                -- Print numeral values instead of "minint/maxint" (NOTE: Custom JDK flag)
                "-XX:+PrintRealMinMax",
                -- Minimal graph-level needed to get the correct graphs
                "-XX:PrintIdealGraphLevel=1",
                -- Output XML file into "<javaClass>.xml"
                "-XX:PrintIdealGraphFile=" ++ javaClass ++ ".xml",
                javaFile
              ]
        outputPath <- liftIO getCurrentDirectory
        (exitCode, _, stdErr) <-
          liftIO $
            readCreateProcessWithExitCode
              ( (proc javaBin compileCommands)
                  { cwd = Just outputPath,
                    std_out = CreatePipe
                  }
              )
              ""
        if exitCode /= ExitSuccess
          then throwError $ runtimeError $ "Exited with code " ++ show exitCode ++ ": " ++ stdErr
          else do
            contents <- liftIO $ readFile (javaClass ++ ".xml")
            -- liftIO $ removeFile (javaClass ++ ".xml")
            return $ contents

-- | Given a seed and name, generates a Java file with the given seed and class name
fuzzProgram :: Word64 -> String -> ErrorM String
fuzzProgram seed className =
  case program seed className "method" of
    Left _ -> throwError $ fuzzError $ "Fuzzer: Failed to generate program with seed: " <> show seed
    Right prog -> return $ show $ pretty prog
