{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Bungalow
import Bungalow.Row
import Data.Int
import Test.Hspec

type DB = '[Schema "users" '[ '("email", Int32), '("name", Int32)]]

access :: Access DB (Row ['("email", Int32), '("name", Int32)])
access = do
  eval insertUser
  eval selectUsers
  where
    insertUser = insert #users ((1 :: Int32) :& (2 :: Int32))
    selectUsers = select (col #email :& col #name) #users

main :: IO ()
main = hspec $ do
  describe "Bungalow.select" $ do
    it "returns a row" $ do
      users <- runAccess_ access
      users `shouldBe` toRow ((1 :: Int32) :& (2 :: Int32))
