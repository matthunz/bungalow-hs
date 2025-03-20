{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Int (Int32)
import MyLib

main :: IO ()
main = do
  t <- newTable @'[ '("x", Int32), '("y", Int32), '("z", Int32)]
  t' <- insert ((1 :: Int32) :& (2 :: Int32) :& (3 :: Int32)) t
  x <- select @'["y", "x"] t'
  print x
  let db = database "users" t'
  run "SELECT y FROM users" db
