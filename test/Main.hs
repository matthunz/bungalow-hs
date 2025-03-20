{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Bungalow
import Bungalow.Row
import Data.Int
import Test.Hspec

type DB = '[Schema "zombies" '[ '("health", Int32), '("damage", Int32)]]


main :: IO ()
main = hspec $ do
  describe "Bungalow.select" $ do
    it "queries an empty row" $ do
      users <- runAccess_ @DB $ eval selectUsers
      users `shouldBe` []
    it "returns a row" $ do
      users <- runAccess_ @DB $ do
        eval insertUser
        eval selectUsers
      users `shouldBe` [toRow ((1 :: Int32) :& (2 :: Int32))]
  where
    insertUser = insert #zombies ((1 :: Int32) :& (2 :: Int32))
    selectUsers = select (col #health :& col #damage) #zombies
