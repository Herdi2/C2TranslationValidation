module Verifier.ErrorHandler where

import Control.Exception
import Control.Monad.IO.Class
import Data.SBV

data ErrorType = ParseError | BuilderError | RuntimeError | FuzzError | VerificationError deriving (Show, Eq)

data CustomError = CustomError {_errorType :: ErrorType, _errorMessage :: String} deriving (Eq)

instance Show CustomError where
  show (CustomError errorType errorMsg) =
    show errorType <> ": " <> show errorMsg

data VerifyException = VerifyException CustomError deriving (Show, Eq)

instance Exception VerifyException

parseError :: String -> CustomError
parseError = CustomError ParseError

builderError :: String -> CustomError
builderError = CustomError BuilderError

verificationError :: String -> CustomError
verificationError = CustomError VerificationError

runtimeError :: String -> CustomError
runtimeError = CustomError RuntimeError

fuzzError :: String -> CustomError
fuzzError = CustomError FuzzError

raiseException :: String -> Symbolic a
raiseException = liftIO . throwIO . VerifyException . CustomError VerificationError
