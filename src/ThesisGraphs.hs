module ThesisGraphs where

import Verifier.Graph

{-
- Incorrect if-guard subsuming (ifnode.cpp#1690)
-
- int method(int x) {
-   if (x >= 0) {
-     if (x <= 0) {
-       // Incorrectly subsumed
-       return 0;
-     } else {
  -     return 1;
-     }
-   } else {
-     return 2;
-   }
- }
-}
controlIfSubsuming :: (Graph, Graph)
controlIfSubsuming =
  (,)
    ( mkGraph
        JINT
        [ -- Data flow
          (11, ParmI 11),
          (23, ConI 0),
          (24, CmpI 11 23),
          (25, Bool Lt 24),
          (32, Bool Le 24),
          (38, ConI 1),
          (39, ConI 2),
          (18, Phi 14 [23, 38, 39]),
          -- Control flow
          (5, ParmCtrl 5),
          (26, If 26 25),
          (28, IfFalse 28),
          (27, IfTrue 27),
          (33, If 33 32),
          (34, IfTrue 34),
          (35, IfFalse 35),
          (14, Region 14 [34, 35, 27]),
          (40, Return 40 18)
        ]
        [ (5, [26]),
          (26, [28, 27]),
          (28, [33]),
          (33, [34, 35]),
          (34, [14]),
          (35, [14]),
          (27, [14]),
          (14, [40])
        ]
    )
    ( mkGraph
        JINT
        [ -- Data flow
          (11, ParmI 11),
          (23, ConI 0),
          (24, CmpI 11 23),
          (25, Bool Lt 24),
          (38, ConI 1),
          (39, ConI 2),
          (18, Phi 14 [38, 39]),
          -- Control flow
          (5, ParmCtrl 5),
          (26, If 26 25),
          (28, IfFalse 28),
          (27, IfTrue 27),
          (14, Region 14 [28, 27]),
          (40, Return 40 18)
        ]
        [ (5, [26]),
          (26, [28, 27]),
          (28, [14]),
          (27, [14]),
          (14, [40])
        ]
    )
