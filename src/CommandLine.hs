module CommandLine where

import Data.Word
import Options.Applicative

data Command
  = Verify VerifyOpts
  | Fuzz FuzzOpts

data VerifyOpts = VerifyOpts
  { verifyFile :: FilePath,
    verifyMethod :: String,
    verifyShowModel :: Bool,
    verifyOutput :: Maybe FilePath
  }

data FuzzOpts = FuzzOpts
  { fuzzOutput :: FilePath,
    fuzzNumber :: Word64,
    fuzzSeed :: Maybe Word64
  }

-- Parsers

verifyOpts :: Parser VerifyOpts
verifyOpts =
  VerifyOpts
    <$> argument str (metavar "<FILE>" <> help "Verify a Java file")
    <*> argument str (metavar "<METHOD>" <> help "The name of the method to compile in the Java file.")
    <*> switch
      ( long "show-model"
          <> help "Show the model on success"
      )
    <*> optional
      ( strOption
          ( long "output"
              <> metavar "<FILE>"
              <> help "Write output to this file"
          )
      )

fuzzOpts :: Parser FuzzOpts
fuzzOpts =
  FuzzOpts
    <$> argument str (metavar "<DIR>" <> help "Directory where all generated Java files and SMT results will go")
    <*> ( option
            auto
            ( long "number"
                <> metavar "INT"
                <> value 20
                <> help "Number of fuzz tests to run"
            )
        )
    <*> optional
      ( option
          auto
          ( long "seed"
              <> metavar "INT"
              <> help "Fuzz a single program with a given seed"
          )
      )

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "verify"
        (info (Verify <$> verifyOpts) (progDesc "Verify a Java file"))
        <> command
          "fuzz"
          (info (Fuzz <$> fuzzOpts) (progDesc "Run the fuzzer"))
    )
