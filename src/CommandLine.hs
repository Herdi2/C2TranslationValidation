module CommandLine where

import Options.Applicative

data Commands = Commands
  { verify :: Maybe String,
    methodName :: String
  }

commands :: Parser Commands
commands =
  Commands
    <$> verify'
    <*> methodName'

verify' :: Parser (Maybe String)
verify' =
  optional $
    strOption
      ( long "verify"
          <> short 'v'
          <> metavar "JAVA_FILE"
          <> help "Verifies the given Java file"
      )

methodName' :: Parser String
methodName' =
  strOption
    ( long "method-name"
        <> metavar "METHOD_NAME"
        <> value "method"
        <> help "Sets the method to compile in the given Java file for --verify"
    )

optionsParser :: ParserInfo Commands
optionsParser =
  info
    (commands <**> helper)
    ( fullDesc
        <> progDesc "Verifies the HotSpot JVM C2 compiler using translation validation."
        <> header "C2 Translation Validation"
    )
