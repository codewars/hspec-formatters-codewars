# Test.Hspec.Formatters.Codewars

Hspec formatter for [Qualified](https://www.qualified.io/) and [Codewars](https://www.codewars.com).

## Usage

Specify `codewars` formatter:

```haskell
-- test/Main.hs
module Main where

import Test.Hspec.Runner
import Test.Hspec.Formatters.Codewars (codewars)

import qualified Spec

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just codewars} Spec.spec
```

```haskell
-- test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=Spec #-}
```

## Development

```shell
cabal install
cabal test
```
