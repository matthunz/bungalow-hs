{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Bungalow
import Bungalow.Row
import Data.Int
import Test.Hspec

type DB = '[Schema "users" '[ '("email", Int32), '("name", Int32)]]

main :: IO ()
main = hspec $ do
  describe "Bungalow.select" $ do
    it "queries an empty row" $ do
      users <- runAccess_ @DB $ eval selectUsers
      users `shouldBe` Nothing
    it "returns a row" $ do
      users <- runAccess_ @DB $ do
        eval insertUser
        eval selectUsers
      users `shouldBe` Just (toRow ((1 :: Int32) :& (2 :: Int32)))
  where
    insertUser = insert #users ((1 :: Int32) :& (2 :: Int32))
    selectUsers = select (col #email :& col #name) #users
