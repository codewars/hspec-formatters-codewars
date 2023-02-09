module Main (main) where

import Test.Hspec.Runner
import Test.Hspec.Formatters.Codewars (codewars)
import Test.Hspec.Core.Formatters.V1 (formatterToFormat)

import qualified Spec

main :: IO ()
main = hspecWith defaultConfig {configFormat = Just $ formatterToFormat codewars} Spec.spec
