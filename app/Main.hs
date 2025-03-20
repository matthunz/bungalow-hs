{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Bungalow
import Data.Int

type DB = '[Schema "users" '[ '("email", Int32), '("name", Int32)]]

main :: IO ()
main = do
  db <- newDatabase @DB

  -- Evaluate queries directly on the database
  db' <- eval insertUser db
  users <- eval selectUsers db'
  print users

  -- Or convert queries to SQL
  let sql = toSql @DB selectUsers
  print sql -- "SELECT email, name FROM users"

  -- And then run them on your database from anywhere
  run sql db
  where
    insertUser = insert #users ((1 :: Int32) :& (2 :: Int32))
    selectUsers = select (field #email :& field #name) #users
