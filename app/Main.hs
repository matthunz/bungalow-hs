{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Bungalow
import Control.Monad.IO.Class
import Data.Int

type DB = '[Schema "users" '[ '("email", Int32), '("name", Int32)]]

access :: Access DB ()
access = do
  -- Evaluate queries directly against your database
  eval insertUser
  users <- eval selectUsers
  liftIO $ print users

  -- Or convert queries to SQL
  let sql = toSql @DB selectUsers
  liftIO $ print sql -- "SELECT email, name FROM users"

  -- And then run them on your database from anywhere
  runSql sql
  where
    insertUser = insert #users ((1 :: Int32) :& (2 :: Int32))
    selectUsers = select (col #email :& col #name) #users

main :: IO ()
main = runAccess_ access
