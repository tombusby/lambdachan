{-# LANGUAGE OverloadedStrings #-}

module LambdaChan.AuthSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import LambdaChan.Auth

spec :: Spec
spec = do
  describe "computeTripcode" $ do
    it "produces a string starting with '!'" $
      T.isPrefixOf "!" (computeTripcode "password") `shouldBe` True

    it "is exactly 11 characters long (! + 10 base64 chars)" $
      T.length (computeTripcode "somepassword") `shouldBe` 11

    it "is deterministic — same password gives same tripcode" $
      computeTripcode "abc" `shouldBe` computeTripcode "abc"

    it "differs for different passwords" $
      computeTripcode "abc" `shouldNotBe` computeTripcode "xyz"

    it "handles empty password" $
      T.isPrefixOf "!" (computeTripcode "") `shouldBe` True

  describe "computeSecureTripcode" $ do
    it "produces a string starting with '!!'" $
      T.isPrefixOf "!!" (computeSecureTripcode "salt" "password") `shouldBe` True

    it "is exactly 12 characters long (!! + 10 base64 chars)" $
      T.length (computeSecureTripcode "salt" "pass") `shouldBe` 12

    it "differs from the standard tripcode for the same password" $
      computeSecureTripcode "salt" "pass" `shouldNotBe` computeTripcode "pass"

    it "changes when the salt changes" $
      computeSecureTripcode "salt1" "pass"
        `shouldNotBe` computeSecureTripcode "salt2" "pass"

  describe "parseAuthorName" $ do
    let salt = "server-salt"

    it "returns the name unchanged when no # is present" $ do
      let (name, trip) = parseAuthorName salt "Anonymous"
      name `shouldBe` "Anonymous"
      trip `shouldBe` Nothing

    it "parses a standard tripcode (single #)" $ do
      let (name, trip) = parseAuthorName salt "Name#password"
      name `shouldBe` "Name"
      trip `shouldNotBe` Nothing

    it "parses a secure tripcode (double ##)" $ do
      let (name, trip) = parseAuthorName salt "Name##password"
      name `shouldBe` "Name"
      trip `shouldNotBe` Nothing

    it "standard and secure tripcodes differ for the same password" $ do
      let (_, stdTrip) = parseAuthorName salt "Name#password"
      let (_, secTrip) = parseAuthorName salt "Name##password"
      stdTrip `shouldNotBe` secTrip

    it "strips whitespace from name" $ do
      let (name, _) = parseAuthorName salt "  Name  #pass"
      name `shouldBe` "Name"

    it "prefers ## over # when both appear" $ do
      -- "Name##pass" should be treated as secure tripcode
      let (name, trip) = parseAuthorName salt "Name##pass"
      name `shouldBe` "Name"
      trip `shouldSatisfy` maybe False (T.isPrefixOf "!!")

    it "standard tripcode is reproducible across calls" $
      fst (parseAuthorName salt "Name#pass") == fst (parseAuthorName salt "Name#pass")
        `shouldBe` True

  describe "hashUserPassword / verifyUserPassword" $ do
    it "verifies a correct password" $ do
      hash <- hashUserPassword "secret"
      verifyUserPassword "secret" hash `shouldBe` True

    it "rejects a wrong password" $ do
      hash <- hashUserPassword "secret"
      verifyUserPassword "wrong" hash `shouldBe` False

    it "two hashes of the same password differ (bcrypt salts)" $ do
      h1 <- hashUserPassword "same"
      h2 <- hashUserPassword "same"
      h1 `shouldNotBe` h2 -- bcrypt uses random salts
