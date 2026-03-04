{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Utils where

import Control.Monad
import Control.Monad.Except
import Data.List
import Data.SBV
import Debug.Trace
import Effectful
import Effectful.Fail (Fail, runFail)
import Fuzzer.Gen
import Prettyprinter (Pretty, pretty)
import System.Directory (getCurrentDirectory, removeFile)
import System.Exit
import System.Process
import qualified System.Random as System
import Verifier.Graph
import Verifier.GraphBuilder
import Verifier.GraphParser
import Verifier.Verify

type ErrorM = Eff '[Fail, IOE]

runErrorM :: ErrorM a -> IO (Either String a)
runErrorM = runEff . runFail

-- | Given a Java file and a method within, return the output of the compiler and the interpreter
-- NOTE: We assume the class who's method we'll compile shared the name with the Java file
compareOutput :: String -> String -> IO (Either String (String, String))
compareOutput javaFile methodName =
  case extractClassName javaFile of
    Nothing -> return $ Left $ "Invalid Java file " <> javaFile
    Just (_path, javaClass) ->
      do
        let compileCommands =
              [ "-Xcomp", -- Compiler only
                "-Xbatch", -- Makes sure compilation finishes
                "-XX:-TieredCompilation", -- C2 only
                "-XX:CompileCommand=compileonly," ++ javaClass ++ "::" ++ methodName, -- Compile only `javaClass::method`
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
createGraph :: String -> String -> Either String Graph
createGraph xmlContent graphName =
  do
    parsedGraph <- parseGraph graphName xmlContent
    builtGraph <- buildGraph parsedGraph
    return builtGraph

-- | Given XML content representing the C2 IR, parses the "After Parsing" and "Before Matching"
-- graphs into internal graph representations.
parseGraphs :: String -> Either String (Graph, Graph)
parseGraphs xmlContent =
  do
    beforeGraph <- parseGraph "After Parsing" xmlContent
    afterGraph <- parseGraph "Before Matching" xmlContent
    before <- buildGraph beforeGraph
    after <- buildGraph afterGraph
    return (before, after)

-- | Verifies the given XML file
verifyXML :: String -> ErrorM String
verifyXML xmlContent =
  case (parseGraphs xmlContent) of
    Left err -> fail err
    Right (before, after) ->
      show <$> (liftIO $ runVerification False before after)

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
compileJavaProgram :: String -> String -> ErrorM String
compileJavaProgram javaFile methodName =
  case extractClassName javaFile of
    Nothing -> fail $ "Invalid Java file " <> javaFile
    Just (_path, javaClass) ->
      do
        let compileCommands =
              [ -- Use compiler only
                "-Xcomp",
                -- Make sure compilation finishes before execution
                "-Xbatch", -- Makes sure compilation finishes
                -- Compile with C2 only
                "-XX:-TieredCompilation", -- C2 only
                -- Compile only `javaClass::method`
                "-XX:CompileCommand=compileonly," ++ javaClass ++ "::" ++ methodName,
                -- Minimal graph-level needed to get the correct gra[hs
                "-XX:PrintIdealGraphLevel=1",
                -- Output XML file into "<javaClass>.xml"
                "-XX:PrintIdealGraphFile=" ++ javaClass ++ ".xml",
                javaFile
              ]
        outputPath <- liftIO getCurrentDirectory
        (exitCode, _, stdErr) <-
          liftIO $
            readCreateProcessWithExitCode
              ( (proc "java" compileCommands)
                  { cwd = Just outputPath,
                    std_out = CreatePipe
                  }
              )
              ""
        if exitCode /= ExitSuccess
          then fail $ "Exited with code " ++ show exitCode ++ ": " ++ stdErr
          else do
            contents <- liftIO $ readFile (javaClass ++ ".xml")
            -- liftIO $ removeFile (javaClass ++ ".xml")
            return $ contents

-- | Given a seed and name, generates a Java file with the given seed and class name
fuzzProgram :: Word64 -> String -> ErrorM String
fuzzProgram seed className =
  case program seed className "method" of
    Left _ -> fail $ "Fuzzer: Failed to generate program with seed: " <> show seed
    Right prog -> return $ show $ pretty prog

verifyProgram :: String -> String -> ErrorM String
verifyProgram javaFile method =
  do
    xmlContent <- compileJavaProgram javaFile method
    verifyXML xmlContent
