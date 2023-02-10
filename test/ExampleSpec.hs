module ExampleSpec where

import Test.Hspec
import Test.QuickCheck

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
    it "explicitly trigerred error" $ do
      error "This test fails with an error"
      read "10" `shouldBe` (10 :: Int)
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
    it "Can present multiline assertions" $ do
      (show 10) `shouldBe`  "10, but\nbroken into\nmultiple lines"
  describe "QuickCheck tests" $ do
    it "passes for random ints" $ do
      property $ \ n -> do
        ((read (show n))::Int) `shouldBe` (n :: Int)
    it "fails for random ints" $ do
      property $ \ n -> do
        ((read (show n))::Int) `shouldBe` ((if n < 50 then n else n + 1) :: Int)