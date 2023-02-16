module Test.Hspec.Formatters.CodewarsSpec (spec) where

import Helper (captureLines, normalizeSummary)
import Test.Hspec
import Test.Hspec.Core.Formatters.V2 (formatterToFormat)
import qualified Test.Hspec.Core.Runner as H
import qualified Test.Hspec.Core.Spec as H
import Test.Hspec.Formatters.Codewars (newFormatter)

runSpecWithCodewars :: Spec -> IO [String]
runSpecWithCodewars s =
  do
    codewars <- newFormatter
    let config = H.defaultConfig {H.configFormat = Just $ formatterToFormat codewars}
    captureLines $ H.hspecWithResult config s
    >>= (return . normalizeSummary)

spec :: Spec
spec = do
  describe "Codewars Formatter" $ do
    it "handles passing tests" $ do
      r <- runSpecWithCodewars $ do
        H.describe "Example" $ do
          H.it "success" (H.Result "" H.Success)
      r
        `shouldBe` [ "",
                     "<DESCRIBE::>Example",
                     "",
                     "<IT::>success",
                     "",
                     "<PASSED::>Test Passed",
                     "",
                     "<COMPLETEDIN::>0.000",
                     "",
                     "<COMPLETEDIN::>0.000"
                   ]

    it "handles failing tests" $ do
      r <- runSpecWithCodewars $ do
        H.describe "parsing" $ do
          H.it "can parse integers and fail" $ do
            read "10" `shouldBe` (11 :: Int)
          H.it "can parse floating-point numbers and fail" $ do
            read "2.5" `shouldBe` (2.6 :: Float)
      r
        `shouldBe` [ "",
                     "<DESCRIBE::>parsing",
                     "",
                     "<IT::>can parse integers and fail",
                     "",
                     "<FAILED::>expected: 11<:LF:> but got: 10",
                     "",
                     "<COMPLETEDIN::>0.000",
                     "",
                     "<IT::>can parse floating-point numbers and fail",
                     "",
                     "<FAILED::>expected: 2.6<:LF:> but got: 2.5",
                     "",
                     "<COMPLETEDIN::>0.000",
                     "",
                     "<COMPLETEDIN::>0.000"
                   ]

    it "handles crashing tests" $ do
      r <- runSpecWithCodewars $ do
        H.it "crash" $ do
          read (last $ tail $ ["10"]) `shouldBe` (11 :: Int)
      r
        `shouldBe` [ "",
                     "<IT::>crash",
                     "",
                     "<ERROR::>uncaught exception: ErrorCall<:LF:>Prelude.last: empty list",
                     "",
                     "<COMPLETEDIN::>0.000"
                   ]

    it "handles pending tests" $ do
      r <- runSpecWithCodewars $ do
        H.it "not implemented" $ do
          H.pending
        H.it "not implemented, with a message" $ do
          H.pendingWith "Not implemented yet"

      r
        `shouldBe` [ "",
                     "<IT::>not implemented",
                     "",
                     "<FAILED::>Test pending: no reason given",
                     "",
                     "<COMPLETEDIN::>0.000",
                     "",
                     "<IT::>not implemented, with a message",
                     "",
                     "<FAILED::>Test pending: Not implemented yet",
                     "",
                     "<COMPLETEDIN::>0.000"
                   ]

    it "handles multiline names" $ do
      r <- runSpecWithCodewars $ do
        H.describe "Can present\nmultiline titles\nof groups" $ do
          H.it "Can present\nmultiline titles\nof items" $ do
            read "10" `shouldBe` (10 :: Int)
      r
        `shouldBe` [ "",
                     "<DESCRIBE::>Can present<:LF:>multiline titles<:LF:>of groups",
                     "",
                     "<IT::>Can present<:LF:>multiline titles<:LF:>of items",
                     "",
                     "<PASSED::>Test Passed",
                     "",
                     "<COMPLETEDIN::>0.000",
                     "",
                     "<COMPLETEDIN::>0.000"
                   ]

    it "handles multiline assertions" $ do
      r <- runSpecWithCodewars $ do
        H.it "present multiline assertions" $ do
          "10" `shouldBe` "10, but\nbroken into\nmultiple lines"
      r
        `shouldBe` [ "",
                     "<IT::>present multiline assertions",
                     "",
                     "<FAILED::>expected: \"10, but\\nbroken into\\nmultiple lines\"<:LF:> but got: \"10\"",
                     "",
                     "<COMPLETEDIN::>0.000"
                   ]
