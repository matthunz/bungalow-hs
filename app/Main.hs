{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Bungalow
import Bungalow.Database (Schema, field, newDatabase, run)
import Bungalow.Row
import Data.Int

type DB = '[Schema "users" '[ '("email", Int32), '("name", Int32)]]

main :: IO ()
main = do
  db <- newDatabase @DB

  -- Evaluate queries directly on the database
  db' <- eval q db
  x <- eval q' db'
  print x

  -- Or convert queries to SQL
  let sql = toSql @DB q'
  print sql -- "SELECT email, name FROM users"

  -- And then run them on your database from anywhere
  run sql db
  where
    q = insert #users ((1 :: Int32) :& (2 :: Int32))
    q' = select (field #email :& field #name) #users
