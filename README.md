# Test.Hspec.Formatters.Codewars

Hspec formatter for [Qualified](https://www.qualified.io/) and [Codewars](https://www.codewars.com).

## Usage

Specify `codewars` formatter:

```haskell
-- test/Main.hs
module Main (main) where

import Test.Hspec.Runner
import Test.Hspec.Formatters.Codewars (newFormatter)
import Test.Hspec.Core.Formatters.V2 (formatterToFormat)

import qualified Spec

main :: IO ()
main = do
  codewars <- newFormatter
  runSpec Spec.spec defaultConfig {configFormat = Just $ formatterToFormat codewars}
  >>= evaluateSummary
```

```haskell
-- test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=Spec #-}
```

## Development

```shell
stack build
stack test
```
