{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.Formatters.Codewars (codewars) where

import Data.Text (pack, replace, unpack)

import Test.Hspec.Core.Util (
  Path,
  joinPath
 )

import Test.Hspec.Core.Formatters.V2 (
  FailureReason (..),
  Formatter (..),
  Item (..),
  Result (..),
  formatException,
  silent,
  writeLine
 )

getName :: Path -> String
getName (_, req) = escapeLF req

codewars :: Formatter
codewars =
  silent
    {
		 formatterGroupStarted = \path -> do
			writeLine ""
			writeLine $ escapeLF $ "<DESCRIBE::>" ++ (getName path)
		,formatterGroupDone = \_ -> do
			writeLine ""
			writeLine $ "<COMPLETEDIN::>"
		,formatterItemStarted = \path -> do
			writeLine ""
			writeLine $ escapeLF $ "<IT::>" ++ (getName path)
		,formatterItemDone = \path item -> do
			writeLine ""
			writeLine $ reportItem item
			writeLine ""
			writeLine $ "<COMPLETEDIN::> " ++ (show $ itemDuration item)
    }

reportItem :: Item -> String
reportItem item =
	case itemResult item of
		Success -> "<PASSED::>Test Passed"
		Pending _ _ -> "\nPENDING"
		Failure _ reason -> reasonAsString reason

reasonAsString :: FailureReason -> String
reasonAsString reason =
  case reason of
    NoReason -> "<FAILED::>Test Failed"
    Reason x -> "<FAILED::>" ++ (escapeLF x)
    ExpectedButGot Nothing expected got ->
      "<FAILED::>Expected " ++ (escapeLF expected) ++ " but got " ++ (escapeLF got)
    ExpectedButGot (Just src) expected got ->
      "<FAILED::>" ++ (escapeLF src) ++ " expected " ++ (escapeLF expected) ++ " but got " ++ (escapeLF got)
    Error Nothing err ->
      "<ERROR::>" ++ (escapeLF $ formatException err)
    Error (Just s) err ->
      "<ERROR::>" ++ (escapeLF s) ++ (escapeLF $ formatException err)

escapeLF :: String -> String
escapeLF = unpack . replace "\n" "<:LF:>" . pack
