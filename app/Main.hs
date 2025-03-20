{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Bungalow
import Bungalow.Row
import Bungalow.Database hiding (insert, select)
import Data.Int

type DB = '[Schema "users" '[ '("x", Int32), '("y", Int32), '("z", Int32)]]

main :: IO ()
main = do
  db <- newDatabase @DB
  let q = insert #users ((1 :: Int32) :& (2 :: Int32) :& (3 :: Int32)) 
      q' = select (field #x :& field #y) #users
  print $ toSql @DB q
  print $ toSql @DB q'
  db' <- eval q db
  x <- eval q' db'
  print x
  run "SELECT y FROM users" db
