{-# LANGUAGE LambdaCase #-}

module Utils where

import Control.Monad
import Data.List
import Data.SBV
import Debug.Trace
import Fuzzer.Gen
import Prettyprinter (pretty)
import System.Exit
import System.Process
import qualified System.Random as System
import Verifier.Graph
import Verifier.GraphBuilder
import Verifier.GraphParser
import Verifier.Verify

-- | Given a Java class name and a method within, return the output of the compiler and the interpreter
compareOutput :: String -> String -> IO (Either String (String, String))
compareOutput javaClass methodName =
  do
    let compileCommands =
          [ "-Xcomp", -- Compiler only
            "-Xbatch", -- Makes sure compilation finishes
            "-XX:-TieredCompilation", -- C2 only
            "-XX:CompileCommand=compileonly," ++ javaClass ++ "::" ++ methodName, -- Compile only `javaClass::method`
            javaClass ++ ".java"
          ]
        interpreterCmds =
          [ "-Xint",
            javaClass ++ ".java"
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
verifyXML :: String -> IO (Either String SatResult)
verifyXML fileName =
  do
    content <- readFile fileName
    case (parseGraphs content) of
      Left err -> return $ Left err
      Right (before, after) -> Right <$> runVerification before after

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

-- | Given a Java file (with absolute path), a method name within the class, and an output path,
-- generates an XML file corresponding to the C2 IR when compiling the given method within the class.
-- Returns the generated XML file on success, and the error code on failure.
compileJavaProgram :: String -> String -> String -> IO (Either String String)
compileJavaProgram javaFile methodName outputPath =
  case extractClassName javaFile of
    Nothing -> return $ Left $ "Invalid Java file " <> javaFile
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
        (exitCode, _, stdErr) <-
          readCreateProcessWithExitCode
            ( (proc "java" compileCommands)
                { cwd = Just outputPath,
                  std_out = CreatePipe
                }
            )
            ""
        if exitCode /= ExitSuccess
          then return $ Left $ "Exited with code " ++ show exitCode ++ ": " ++ stdErr
          else return $ Right $ javaClass ++ ".xml"

-- | Generates a program and writes to the output path, returning the name of the written file
-- on success, or the seed on failure.
fuzzProgram :: String -> IO (Either String String)
fuzzProgram outPath =
  do
    seed <- System.randomIO
    let className = "Klass" <> show seed
    case program seed className "method" of
      Left _ -> return $ Left $ "Fuzzer: Failed to generate program with seed: " <> show seed
      Right prog ->
        do
          let fileName = outPath ++ className ++ ".java"
          writeFile fileName (show $ pretty prog)
          return $ Right fileName
