{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad
import Data.List
import Data.SBV
import Graph
import GraphBuilder
import GraphParser
import System.Exit
import System.Process
import Verify

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

-- | Given a Java class name, a method name within the class, and an output path,
-- generates an XML file corresponding to the C2 IR when compiling the given method within the class.
-- Returns the generated XML file on success, and the error code on failure.
compileJavaProgram :: String -> String -> String -> IO (Either String String)
compileJavaProgram javaClass methodName outputPath =
  do
    let compileCommands =
          [ "-Xcomp", -- Compiler only
            "-Xbatch", -- Makes sure compilation finishes
            "-XX:-TieredCompilation", -- C2 only
            "-XX:CompileCommand=compileonly," ++ javaClass ++ "::" ++ methodName, -- Compile only `javaClass::method`
            "-XX:PrintIdealGraphLevel=1",
            "-XX:PrintIdealGraphFile=" ++ javaClass ++ ".xml",
            javaClass ++ ".java"
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

main :: IO ()
main =
  do
    let javaClass = "AndNeg"
        methodName = "method"
        path = "/home/herdi/Desktop/master-work/C2TranslationValidation"
    compileJavaProgram javaClass methodName path
      >>= \case
        Left err -> putStrLn err
        Right xml ->
          verifyXML xml
            >>= \case
              Left err -> putStrLn err
              Right res ->
                do
                  print res
                  compareOutput javaClass methodName
                    >>= \case
                      Left err -> putStrLn err
                      Right res -> print res

andNegBefore :: Graph
andNegBefore =
  mkGraph
    ( [ (10, ParmI 10),
        (11, ParmI 11),
        (23, ConI 0),
        (24, SubI 23 10),
        (25, SubI 23 11),
        (26, AndI 24 25),
        (27, Return 27 26)
      ]
    )
    [(5, [27])]

andNegAfter :: Graph
andNegAfter =
  mkGraph
    ( [ (10, ParmI 10),
        (11, ParmI 11),
        (26, AndI 10 11),
        (27, Return 27 26)
      ]
    )
    [(5, [27])]

sideBefore :: Graph
sideBefore =
  mkGraph
    ( [ (10, ParmI 10),
        (11, ParmI 11),
        (23, ConI 0),
        (24, SubI 23 10),
        (25, SubI 23 11),
        (26, AndI 24 25),
        (27, ConI (-4)),
        (28, CmpI 26 27),
        (29, Bool Ne 28),
        (5, ParmCtrl 5),
        (30, If 30 29),
        (31, IfTrue 31),
        (32, IfFalse 32),
        (14, Region 14 [32, 31]),
        (36, ConI 120),
        (37, ConI 140),
        (18, Phi 14 [36, 37]),
        (38, Return 38 18)
      ]
    )
    ( [ (5, [30]),
        (30, [31, 32]),
        (32, [14]),
        (31, [14]),
        (14, [38])
      ]
    )

sideAfter :: Graph
sideAfter =
  mkGraph
    ( [ (10, ParmI 10),
        (11, ParmI 11),
        (26, AndI 10 11),
        (27, ConI (-4)),
        (28, CmpI 26 27),
        (29, Bool Ne 28),
        (5, ParmCtrl 5),
        (30, If 30 29),
        (31, IfTrue 31),
        (32, IfFalse 32),
        (14, Region 14 [32, 31]),
        (36, ConI 120),
        (37, ConI 140),
        (18, Phi 14 [36, 37]),
        (38, Return 38 18)
      ]
    )
    ( [ (5, [30]),
        (30, [31, 32]),
        (32, [14]),
        (31, [14]),
        (14, [38])
      ]
    )

phiBefore :: Graph
phiBefore =
  mkGraph
    ( [ (5, ParmCtrl 5),
        (10, ParmI 10),
        (11, ParmI 11),
        (23, ConI 0),
        (24, SubI 23 10),
        (25, SubI 23 11),
        (26, AndI 24 25),
        (27, ConI (-2)),
        (28, CmpI 26 27),
        (29, Bool Ne 28),
        (5, ParmCtrl 5),
        (30, If 30 29),
        (31, IfTrue 31),
        (32, IfFalse 32),
        (14, Region 14 [32, 31]),
        (36, ConI 140),
        (37, ConI 120),
        (18, Phi 14 [36, 37]),
        (38, Return 38 18)
      ]
    )
    ( [ (5, [30]),
        (30, [31, 32]),
        (32, [14]),
        (31, [14]),
        (14, [38])
      ]
    )

phiAfter :: Graph
phiAfter =
  mkGraph
    ( [ (5, ParmCtrl 5),
        (10, ParmI 10),
        (11, ParmI 11),
        (26, AndI 10 11),
        (27, ConI (-2)),
        (28, CmpI 26 27),
        (29, Bool Ne 28),
        (5, ParmCtrl 5),
        (30, If 30 29),
        (31, IfTrue 31),
        (32, IfFalse 32),
        (14, Region 14 [32, 31]),
        (36, ConI 140),
        (37, ConI 120),
        (18, Phi 14 [36, 37]),
        (38, Return 38 18)
      ]
    )
    ( [ (5, [30]),
        (30, [31, 32]),
        (32, [14]),
        (31, [14]),
        (14, [38])
      ]
    )

testGraph :: Graph
testGraph =
  mkGraph
    ([(1, ConI 14), (2, ConI 15), (3, AddI 1 2), (4, ParmI 4)])
    []

binopGraph :: Graph
binopGraph =
  mkGraph
    ( [ (29, Return 29 28),
        (28, AddI 25 27),
        (25, MulI 10 11),
        (27, AddI 10 10),
        (10, ParmI 10),
        (11, ParmI 11)
      ]
    )
    []

lshiftBefore :: Graph
lshiftBefore =
  mkGraph
    ( [ (10, ParmL 10),
        (23, AddL 10 10),
        (24, ConI 63),
        (25, LShiftL 23 24),
        (26, Return 26 25)
      ]
    )
    []

lshiftAfter :: Graph
lshiftAfter =
  mkGraph
    ( [ (10, ParmL 10),
        (26, Return 26 10)
      ]
    )
    []

mulassoBefore :: Graph
mulassoBefore =
  mkGraph
    ( [ (10, ParmF 10),
        (22, ConF 00111101111110111110011101101101),
        (23, ConF 00111111100111010111000010100100),
        (24, MulF 10 22),
        (25, MulF 24 23),
        (26, Return 26 25)
      ]
    )
    []

mulassoAfter :: Graph
mulassoAfter =
  mkGraph
    ( [ (10, ParmF 10),
        (27, ConF 00111110000110101110101111000100),
        (25, MulF 10 27),
        (26, Return 26 25)
      ]
    )
    []

mulFloatBefore :: Graph
mulFloatBefore =
  mkGraph
    ( [ (5, ParmCtrl 5),
        (10, ParmF 10),
        (22, ConF 0),
        (23, MulF 22 10),
        (24, ConF 01000000000000000000000000000000),
        (28, CmpF 10 24),
        (29, Bool Ne 28),
        (35, ConI 25),
        (36, CallStatic 36 23),
        (30, If 30 29),
        (31, IfTrue 31),
        (32, IfFalse 32),
        (41, Return 41 23)
      ]
    )
    ( [ (5, [30]),
        (30, [31, 32]),
        (32, [36]),
        (31, [41])
      ]
    )

mulFloatAfter :: Graph
mulFloatAfter =
  mkGraph
    ( [ (5, ParmCtrl 5),
        (10, ParmF 10),
        (22, ConF 0),
        (24, ConF 01000000000000000000000000000000),
        (28, CmpF 10 24),
        (29, Bool Ne 28),
        (35, ConI 25),
        (36, CallStatic 36 22),
        (30, If 30 29),
        (31, IfTrue 31),
        (32, IfFalse 32),
        (41, Return 41 22)
      ]
    )
    ( [ (5, [30]),
        (30, [31, 32]),
        (32, [36]),
        (31, [41])
      ]
    )
