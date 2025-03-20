{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Bungalow
import Control.Monad.IO.Class
import Data.Int

type DB = '[Schema "zombies" '[ '("health", Int32), '("damage", Int32)]]

access :: Access DB ()
access = do
  -- Evaluate queries directly against your database
  eval insertZombie
  users <- eval selectZombies
  liftIO $ print users

  -- Or convert queries to SQL
  let sql = toSql @DB selectZombies
  liftIO $ print sql -- "SELECT email, name FROM users"

  -- And then run them on your database from anywhere
  runSql sql
  where
    insertZombie = insert #zombies ((100 :: Int32) :& (20 :: Int32))
    selectZombies = select (col #health :& col #damage) #zombies

main :: IO ()
main = runAccess_ access
