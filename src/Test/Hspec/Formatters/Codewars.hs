{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Formatters.Codewars
  (
    codewars
  ) where

import Data.Text (pack, unpack, replace)

import Test.Hspec.Formatters (Formatter (..),
                              FailureReason (..),
                              formatException,
                              silent,
                              writeLine)

codewars :: Formatter
codewars = silent {
  exampleGroupStarted = \_ name -> do
    writeLine ""
    writeLine $ escapeLF $ "<DESCRIBE::>" ++ name

, exampleGroupDone = writeLine "\n<COMPLETEDIN::>"

, exampleSucceeded = \(_, name) _ -> do
    writeLine ""
    writeLine $ escapeLF $ "<IT::>" ++ name
    writeLine "\n<PASSED::>Test Passed"
    writeLine "\n<COMPLETEDIN::>"

, exampleFailed = \(_, name) _ reason -> do
    writeLine ""
    writeLine $ escapeLF $ "<IT::>" ++ name
    writeLine ""
    writeLine $ escapeLF $ reasonAsString reason
    writeLine "\n<COMPLETEDIN::>"
}

reasonAsString :: FailureReason -> String
reasonAsString reason =
  case reason of
    NoReason -> "<FAILED::>Test Failed"
    Reason x -> "<FAILED::>" ++ x
    ExpectedButGot Nothing expected got ->
      "<FAILED::>Expected " ++ expected ++ " but got " ++ got
    ExpectedButGot (Just src) expected got ->
      "<FAILED::>" ++ src ++ " expected " ++ expected ++ " but got " ++ got
    Error Nothing err ->
      "<ERROR::>" ++ (formatException err)
    Error (Just s) err ->
      "<ERROR::>" ++ s ++ (formatException err)

escapeLF :: String -> String
escapeLF = unpack . replace "\n" "<:LF:>" . pack
