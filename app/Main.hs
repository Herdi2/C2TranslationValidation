{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Utils

main :: IO ()
main =
  do
    let outPath = "/home/herdi/Desktop/master-work/C2TranslationValidation/output/"
    fuzzProgram outPath
      >>= \case
        Left err -> putStrLn err
        Right javaFile ->
          compileJavaProgram javaFile "method" outPath
            >>= \case
              Left err -> putStrLn err
              Right xml ->
                verifyXML (outPath ++ xml)
                  >>= \case
                    Left err -> putStrLn err
                    Right res -> print res >> print javaFile
