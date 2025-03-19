{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module MyLib where

import Foreign

data Row as where
  Nil :: Row '[]
  Cons :: b -> Row bs -> Row (b ': bs)

instance Storable (Row '[]) where
  sizeOf _ = 0
  alignment _ = 1
  peek _ = return Nil
  poke _ Nil = return ()

instance (Storable a, Storable (Row as)) => Storable (Row (a ': as)) where
  sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: Row as)
  alignment _ = alignment (undefined :: a) `max` alignment (undefined :: Row as)
  peek p = do
    a <- peek (castPtr p)
    as <- peek (castPtr (p `plusPtr` sizeOf a))
    return (Cons a as)
  poke p (Cons a as) = do
    poke (castPtr p) a
    poke (castPtr (p `plusPtr` sizeOf a)) as

instance (ShowRow a) => Show (Row a) where
  show r = "[" ++ showRow r

class ShowRow a where
  showRow :: Row a -> String

instance ShowRow '[] where
  showRow Nil = "]"

instance (Show a) => ShowRow '[a] where
  showRow (Cons a Nil) = show a ++ "]"

instance (Show a, ShowRow (b ': bs)) => ShowRow (a ': b ': bs) where
  showRow (Cons a as) = show a ++ ", " ++ showRow as

data Table a = Table
  { tablePtr :: ForeignPtr (Row a),
    tableOffset :: Int,
    tableLength :: Int
  }

newTable :: forall a. (Storable (Row a)) => IO (Table a)
newTable = do
  ptr <- mallocForeignPtrBytes (sizeOf (undefined :: Row a))
  return (Table ptr 0 0)

insert :: forall a. (Storable (Row a)) => Row a -> Table a -> IO (Table a)
insert row table = withForeignPtr (tablePtr table) $ \p -> do
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

lookup :: forall a. (Storable (Row a)) => Int -> Table a -> IO (Row a)
lookup i table = withForeignPtr (tablePtr table) $ \p -> do
  peek (p `plusPtr` (i * sizeOf (undefined :: Row a)))
