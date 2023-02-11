{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (pack, replace, unpack)
import System.Exit

import Test.Hspec.Runner
import Test.Hspec.Formatters.Codewars (codewars)
import Test.Hspec.Core.Formatters.V2 (formatterToFormat, formatException)
import Test.Hspec.Core.Util (safeTry)

import qualified Spec

main :: IO ()
main = do
  summary <- safeTry $ runSpec Spec.spec defaultConfig {configFormat = Just $ formatterToFormat codewars}
  case summary of
    Left ex -> do
      putStrLn $ "\n<ERROR::>Test suite crashed<:LF:>" ++ (escapeLF $ formatException ex)
      exitFailure
    Right s -> evaluateSummary s

escapeLF :: String -> String
escapeLF = unpack . replace "\n" "<:LF:>" . pack
