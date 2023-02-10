module ExampleSpec where

import Test.Hspec

spec :: Spec
spec = describe "ExampleSpec" $ do
  describe "Passing tests" $ do
    it "can parse integers" $ do
      read "10" `shouldBe` (10 :: Int)
    it "can parse floating-point numbers" $ do
      read "2.5" `shouldBe` (2.5 :: Float)
  describe "Failing tests" $ do
    it "can parse integers and fail" $ do
      read "10" `shouldBe` (11 :: Int)
    it "can parse floating-point numbers and fail" $ do
      read "2.5" `shouldBe` (2.6 :: Float)
  describe "Crashing tests" $ do
    it "can parse integers and crash" $ do
      read (last $ tail $ [ "10" ]) `shouldBe` (11 :: Int)
    it "can parse floating-point numbers and crash" $ do
      read (last $ tail $ [ "2.5" ]) `shouldBe` (2.6 :: Float)
--    it ("can parse item title " ++ (show $ ((read "cant parse this")::Int)) ++ " and crash") $ do
--      read "2.5" `shouldBe` (2.5 :: Float)
  describe "Pending tests" $ do
    it "not implemented" $ do
      pending
    it "not implemented, with a message" $ do
      pendingWith "Not implemented yet"
  describe "Can present\nmultiline titles\nof groups" $ do
    it "Can present\nmultiline titles\nof items" $ do
      read "10" `shouldBe` (10 :: Int)