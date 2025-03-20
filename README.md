# Bungalow
[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/bungalow-hs/bungalow/blob/main/LICENSE)
[![Package](https://img.shields.io/hackage/v/bungalow.svg)](https://hackage.haskell.org/package/bungalow)
[![CI status](https://github.com/matthunz/bungalow-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/matthunz/bungalow-hs/actions)

An in-memory relational database for Haskell.

```hs
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
    selectUsers = select (field #email :& field #name) #users

main :: IO ()
main = runAccess_ access
```
