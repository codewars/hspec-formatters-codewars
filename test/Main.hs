{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (pack, replace, unpack)
import System.Exit

import Test.Hspec.Runner
import Test.Hspec.Formatters.Codewars (codewars, escapeLF)
import Test.Hspec.Core.Formatters.V2 (formatterToFormat, formatException)
import Test.Hspec.Core.Util (safeTry)

import qualified Spec

main :: IO ()
main = do
  formatter <- codewars
  summary <- safeTry $ runSpec Spec.spec defaultConfig {configFormat = Just $ formatterToFormat formatter}
  case summary of
    Left ex -> do
      putStrLn $ "\n<ERROR::>Test suite crashed<:LF:>" ++ (escapeLF $ formatException ex)
      exitFailure
    Right s -> evaluateSummary s
