{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Int (Int32)
import Data.Proxy
import MyLib

main :: IO ()
main = do
  t <- newTable @'[ '("x", Int32), '("y", Int32)]
  t' <- insert (Cons (Proxy @"x") 1 (Cons (Proxy @"y") 2 Nil)) t
  let db = database "users" t'
  run "SELECT y FROM users" db
