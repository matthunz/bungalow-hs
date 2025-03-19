{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Int (Int32)
import MyLib

main :: IO ()
main = do
  t <- newTable
  t' <- insert (Cons (1 :: Int32) (Cons (2 :: Int32) Nil)) t
  t'' <- insert (Cons (3 :: Int32) (Cons (4 :: Int32) Nil)) t'
  r <- MyLib.lookup 0 t''
  r' <- MyLib.lookup 1 t''
  print (r, r')
