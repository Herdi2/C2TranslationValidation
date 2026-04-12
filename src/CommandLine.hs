module CommandLine where

import Data.Word
import Options.Applicative

data GlobalOpts
  = GlobalOpts
  { globalJavaBin :: FilePath,
    globalCommand :: Command
  }

data Command
  = Verify VerifyOpts
  | Fuzz FuzzOpts
  | Campaign CampaignOpts

data VerifyOpts = VerifyOpts
  { verifyFile :: FilePath,
    verifyMethod :: String,
    verifyOutput :: Maybe FilePath
  }

data FuzzOpts = FuzzOpts
  { fuzzDir :: Maybe FilePath,
    fuzzSeed :: Maybe Word64
  }

data CampaignOpts = CampaignOpts
  { campaignDir :: FilePath,
    campaignNum :: Int,
    campaignZ3Timeout :: Integer
  }

verifyOpts :: Parser VerifyOpts
verifyOpts =
  VerifyOpts
    <$> argument str (metavar "<FILE>" <> help "Verify a Java file")
    <*> argument str (metavar "<METHOD>" <> help "The name of the method to compile in the Java file.")
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "<FILE>"
              <> help "Write output to this file"
          )
      )

fuzzOpts :: Parser FuzzOpts
fuzzOpts =
  FuzzOpts
    <$> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "<DIR>"
              <> help "Output the generated program to a file in <DIR>"
          )
      )
    <*> optional
      ( option
          auto
          ( long "seed"
              <> metavar "WORD64"
              <> help "Generate a single program with a given seed"
          )
      )

campaignOpts :: Parser CampaignOpts
campaignOpts =
  CampaignOpts
    <$> argument str (metavar "<DIR>" <> help "Directory where all generated Java files and SMT results will go")
    <*> ( option
            auto
            ( long "number"
                <> short 'n'
                <> metavar "<INT>"
                <> value 20
                <> help "Number of fuzz tests to run"
            )
        )
    <*> ( option
            auto
            ( long "timeout"
                <> short 't'
                <> metavar "<INT>"
                <> value (180 * 1000)
                <> help "Timeout for the SMT solver"
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
        <> command
          "campaign"
          (info (Campaign <$> campaignOpts) (progDesc "Run the campaign"))
    )

globalParser :: Parser GlobalOpts
globalParser =
  GlobalOpts
    <$> strOption
      ( long "java"
          <> short 'j'
          <> metavar "<FILE>"
          <> value "java"
          <> showDefault
          <> help "Path to the Java binary to use"
      )
    <*> commandParser
