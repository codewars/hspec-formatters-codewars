module ExampleSpec where

import Test.Hspec

spec :: Spec
spec = describe "ExampleSpec" $ do
  it "can parse integers" $ do
    read "10" `shouldBe` (10 :: Int)

  it "can parse floating-point numbers" $ do
    read "2.5" `shouldBe` (2.5 :: Float)
