{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Int (Int32)
import MyLib

main :: IO ()
main = do
  t <- newTable @'[ '("x", Int32), '("y", Int32)]
  let db = database "users" t
  run "SELECT x, y FROM users" db
