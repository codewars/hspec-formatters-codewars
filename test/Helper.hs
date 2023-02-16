module Helper (captureLines, normalizeSummary) where

import Data.Char
import Data.List (isPrefixOf)
import System.IO.Silently

captureLines :: IO a -> IO [String]
captureLines = fmap lines . capture_

-- Replace duration in summary with zeroes
normalizeSummary :: [String] -> [String]
normalizeSummary = map f
  where
    f x
      | "<COMPLETEDIN::>" `isPrefixOf` x = map g x
      | otherwise = x
    g x
      | isNumber x = '0'
      | otherwise = x
