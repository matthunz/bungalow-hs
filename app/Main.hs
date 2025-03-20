{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Bungalow
import Bungalow.Row
import Data.Int

main :: IO ()
main = do
  db <- toDatabase @'[Schema "users" '[ '("x", Int32), '("y", Int32), '("z", Int32)]]
  db' <- insert @"users" ((1 :: Int32) :& (2 :: Int32) :& (3 :: Int32)) db
  x <- select @"users" @'["y", "x"] db'
  print x
  run "SELECT y FROM users" db
