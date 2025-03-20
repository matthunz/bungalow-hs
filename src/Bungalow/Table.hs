{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bungalow.Table where

import Bungalow.Row
import Data.Kind
import Foreign
import GHC.TypeLits
import Prelude hiding (lookup)

data Table (a :: [(Symbol, Type)]) = Table
  { tablePtr :: ForeignPtr (Row a),
    tableOffset :: Int,
    tableLength :: Int
  }

newTable :: forall a. (Storable (Row a)) => IO (Table a)
newTable = do
  ptr <- mallocForeignPtrBytes (sizeOf (undefined :: Row a))
  return (Table ptr 0 0)

insert :: forall as bs. (ToRow bs as, Storable (Row bs)) => as -> Table bs -> IO (Table bs)
insert as t = insertRow (toRow @bs as) t

insertRow :: forall a. (Storable (Row a)) => Row a -> Table a -> IO (Table a)
insertRow row table = withForeignPtr (tablePtr table) $ \p -> do
  if tableLength table - tableOffset table < sizeOf (undefined :: Row a)
    then do
      poke (p `plusPtr` tableOffset table) row
      return
        table
          { tableOffset = tableOffset table + sizeOf (undefined :: Row a),
            tableLength = tableLength table + sizeOf (undefined :: Row a)
          }
    else do
      ptr <- mallocForeignPtrBytes (sizeOf (undefined :: Row a) * (tableLength table + 1))
      withForeignPtr ptr $ \p' -> do
        copyBytes p' p (tableOffset table)
        free p
        poke (p' `plusPtr` tableOffset table) row
        return
          Table
            { tablePtr = ptr,
              tableOffset = tableOffset table + sizeOf (undefined :: Row a),
              tableLength = tableLength table + sizeOf (undefined :: Row a)
            }

lookup :: forall a b. (Storable (Row a)) => Int -> Table b -> IO (Row a)
lookup i table = withForeignPtr (tablePtr table) $ \p -> do
  peek (p `plusPtr` (i * sizeOf (undefined :: Row a)))
