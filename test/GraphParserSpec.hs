module GraphParserSpec (spec) where

import Verifier.Graph
import Test.Hspec
import Verifier.Utils
import System.Directory

wuTestPath :: String
wuTestPath = "/home/herdi/Desktop/master-work/C2TranslationValidation/java-examples/old-data-bugs/"

testPath :: String
testPath = "/home/herdi/Desktop/master-work/C2TranslationValidation/java-examples/tests/"

-- | Testing graph parsing and building
-- to make sure that graphs are parsed correctly into our internal representation
-- NOTE: We only compare the "Before" graphs, as after may change based on the compiler that is used
spec :: Spec
spec =
  describe "Graph parsing and building" $ do
    describe "From Wu's thesis" $ do
      it "can parse \"AndNeg.java\"" $
        checkGraph wuTestPath "AndNeg.java" "andneg" andNeg
      it "can parse \"Floating.java\"" $
        checkGraph wuTestPath "Floating.java" "floating" floating
      it "can parse \"LShift.java\"" $
        checkGraph wuTestPath "LShift.java" "lshift" lshift
      it "can parse \"MulAsso.java\"" $
        checkGraph wuTestPath "MulAsso.java" "mulasso" mulAsso
      it "can parse \"MulFloat.java\"" $
        checkGraph wuTestPath "MulFloat.java" "mulfloat" mulFloat
    describe "Extended test suite (control flow etc.)" $ do
      it "can parse \"SideEffect.java\"" $
        checkGraph testPath "SideEffect.java" "sideeffect" sideEffect 

checkGraph :: String -> String -> String -> Graph -> IO ()
checkGraph filePath fileName methodName referenceGraph =
    do
      res <- compileJavaProgram (filePath ++ fileName) methodName filePath
      case res of
        Left err -> fail err
        Right xmlFile ->
          do
            xmlContent <- readFile (filePath ++ xmlFile)
            (case createGraph xmlContent "After Parsing" of
              Right graph -> graph `shouldBe` referenceGraph
              Left err -> fail err)
            >> removeFile (filePath ++ xmlFile)

andNeg :: Graph
andNeg =
  mkGraph
    ( [ (5, ParmCtrl 5),
        (10, ParmI 10),
        (11, ParmI 11),
        (23, ConI 0),
        (24, SubI 23 10),
        (25, SubI 23 11),
        (26, AndI 24 25),
        (27, Return 27 26)
      ]
    )
    [(5, [27])]

floating :: Graph
floating =
  mkGraph
    ( [ (5, ParmCtrl 5),
        (10, ParmF 10),
        (22, ConF 00000000000000000000000000000000),
        (23, SubF 22 10),
        (24, SubF 22 23),
        (25, Return 25 24)
      ]
    )
    [(5, [25])]

lshift :: Graph
lshift =
  mkGraph
    ( [ (5, ParmCtrl 5),
        (10, ParmL 10),
        (23, AddL 10 10),
        (24, ConI 63),
        (25, LShiftL 23 24),
        (26, Return 26 25)
      ]
    )
    [(5, [26])]

mulAsso :: Graph
mulAsso =
  mkGraph
    ( [ (5, ParmCtrl 5),
        (10, ParmF 10),
        (22, ConF 00111101111110111110011101101101),
        (23, ConF 00111111100111010111000010100100),
        (24, MulF 10 22),
        (25, MulF 24 23),
        (26, Return 26 25)
      ]
    )
    [(5, [26])]

mulFloat :: Graph
mulFloat =
  mkGraph
    ( [ (5, ParmCtrl 5),
        (10, ParmF 10),
        (22, ConF 0),
        (23, MulF 22 10),
        (24, Return 24 23)
      ]
    )
    ( [ (5, [24])
      ]
    )

sideEffect =
  mkGraph
    ( [ (10, ParmI 10),
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
        (30, [32, 31]),
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
