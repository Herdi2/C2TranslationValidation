{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Verifier.Utils

main :: IO ()
main =
  do
    let javaFile = "AndNeg.java"
        methodName = "method"
        path = "/home/herdi/Desktop/master-work/C2TranslationValidation/java-examples/old-data-bugs/"
    compileJavaProgram javaFile methodName path
      >>= \case
        Left err -> putStrLn err
        Right xml ->
          verifyXML xml
            >>= \case
              Left err -> putStrLn err
              Right res ->
                do
                  print res
                  compareOutput "AndNeg" methodName
                    >>= \case
                      Left err -> putStrLn err
                      Right res -> print res
