{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.Formatters.Codewars (codewars, escapeLF) where

import Data.Text (pack, replace, unpack)
import Text.Printf (printf)
import Data.IORef
import Control.Monad.IO.Class
import Test.Hspec.Core.Util (Path)

import Test.Hspec.Core.Formatters.V2 (
  FailureReason (..),
  Formatter     (..),
  Item          (..),
  Seconds       (..),
  Result        (..),
  formatException,
  silent,
  writeLine,
  getRealTime
 )

getName :: Path -> String
getName (_, req) = escapeLF req

codewars :: IO Formatter
codewars = do
  times <- newIORef ([ ]::[Seconds])
  pure $ silent
    {
      formatterGroupStarted = \path -> do
        writeLine ""
        startedOn <- getRealTime
        liftIO $ modifyIORef times (\ts -> startedOn : ts)
        writeLine $ "<DESCRIBE::>" ++ (getName path)
      ,formatterGroupDone = \_ -> do
        writeLine ""
        ts <- liftIO $ readIORef times
        now <- getRealTime
        let startedOn = head ts
        let duration = now - startedOn
        writeLine $ "<COMPLETEDIN::> " ++ (formatToMillis $ duration)
        liftIO $ modifyIORef times tail
      ,formatterItemStarted = \path -> do
        writeLine ""
        writeLine $ "<IT::>" ++ (getName path)
      ,formatterItemDone = \_ item -> do
        writeLine ""
        writeLine $ reportItem item
        writeLine ""
        writeLine $ "<COMPLETEDIN::>" ++ (formatToMillis $ itemDuration item)
    }

reportItem :: Item -> String
reportItem item =
  case itemResult item of
    Success -> "<PASSED::>Test Passed"
    Failure _ reason -> reasonAsString reason
    Pending _  Nothing -> "<FAILED::>Test pending: no reason given"
    Pending _  (Just msg) -> "<FAILED::>Test pending: " ++ (escapeLF msg)

reasonAsString :: FailureReason -> String
reasonAsString reason =
  case reason of
    NoReason -> "<FAILED::>Test Failed"
    Reason x -> "<FAILED::>" ++ (escapeLF x)
    ExpectedButGot Nothing expected got ->
      "<FAILED::>expected: " ++ (escapeLF expected) ++ "<:LF:> but got: " ++ (escapeLF got)
    ExpectedButGot (Just src) expected got ->
      "<FAILED::>" ++ (escapeLF src) ++ "<:LF:>expected: " ++ (escapeLF expected) ++ "<:LF:> but got: " ++ (escapeLF got)
    Error Nothing err ->
      "<ERROR::>uncaught exception: " ++ (escapeLF $ formatException err)
    Error (Just s) err ->
      "<ERROR::>" ++ (escapeLF s) ++ "<:LF:>" ++ (escapeLF $ formatException err)


formatToMillis :: Seconds -> String
formatToMillis (Seconds s) = printf "%.3f" (s * 1000)

escapeLF :: String -> String
escapeLF = unpack . replace "\n" "<:LF:>" . pack
