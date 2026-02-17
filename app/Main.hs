module Main (main) where

import Data.List
import Graph
import GraphBuilder
import GraphParser
import System.Process
import Verify

compareOutput :: String -> IO String
compareOutput className =
  do
    let compilerCmds =
          [ "-Xcomp",
            "-XX:-TieredCompilation",
            "-XX:CompileCommand=compileonly," ++ className ++ "::method",
            className ++ ".java"
          ]
    let interpreterCmds =
          [ "-Xint",
            className ++ ".java"
          ]
    compilerRes <- readProcess "java" compilerCmds ""
    interpreterRes <- readProcess "java" interpreterCmds ""
    pure $
      unlines $
        [ "Compiler result: ",
          compilerRes,
          "Interpreter result: ",
          interpreterRes
        ]

main :: IO ()
main =
  do
    -- res <- runVerification andNegBefore andNegAfter
    res <- runVerification sideBefore sideAfter
    print res
    compareOutput "AndNeg" >>= putStrLn

andNegBefore :: Graph
andNegBefore =
  mkGraph
    ( [ (10, ParmI 10),
        (11, ParmI 11),
        (23, ConI 0),
        (24, SubI 23 10),
        (25, SubI 23 11),
        (26, AndI 24 25),
        (27, Return 26)
      ]
    )
    [(5, [27])]

andNegAfter :: Graph
andNegAfter =
  mkGraph
    ( [ (10, ParmI 10),
        (11, ParmI 11),
        (26, AndI 10 11),
        (27, Return 26)
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
        (38, Return 18)
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
        (38, Return 18)
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
        (38, Return 18)
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
        (38, Return 18)
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
    ( [ (29, Return 28),
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
        (26, Return 25)
      ]
    )
    []

lshiftAfter :: Graph
lshiftAfter =
  mkGraph
    ( [ (10, ParmL 10),
        (26, Return 10)
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
        (26, Return 25)
      ]
    )
    []

mulassoAfter :: Graph
mulassoAfter =
  mkGraph
    ( [ (10, ParmF 10),
        (27, ConF 00111110000110101110101111000100),
        (25, MulF 10 27),
        (26, Return 25)
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
        (41, Return 23)
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
        (41, Return 22)
      ]
    )
    ( [ (5, [30]),
        (30, [31, 32]),
        (32, [36]),
        (31, [41])
      ]
    )
