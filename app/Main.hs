module Main (main) where

import Verify
import GraphParser

main :: IO ()
main =
  do
    contents <- readFile "SideEffect.xml"
    before <- parseGraph "After Parsing" contents
    after <- parseGraph "Before Matching" contents
    print $ before
    print $ after
