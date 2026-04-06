module Main (main) where

import Test.Hspec

import qualified LambdaChan.AuthSpec     as AuthSpec
import qualified LambdaChan.DatabaseSpec as DatabaseSpec
import qualified LambdaChan.APISpec      as APISpec

main :: IO ()
main = hspec $ do
  describe "Auth"     AuthSpec.spec
  describe "Database" DatabaseSpec.spec
  describe "API"      APISpec.spec
