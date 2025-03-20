{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Bungalow.Table where

import Bungalow.Row
import Data.Kind
import Data.Proxy
import Foreign
import GHC.TypeLits
import Unsafe.Coerce
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

type family SelectFromT' (s :: Symbol) (as :: [(Symbol, Type)]) where
  SelectFromT' s ('(s, a) ': xs) = a
  SelectFromT' s ('(t, a) ': xs) = SelectFromT' s xs

type family SelectFromT (s :: [Symbol]) (as :: [(Symbol, Type)]) where
  SelectFromT '[] as = '[]
  SelectFromT (s ': ss) as = '(s, SelectFromT' s as) ': SelectFromT ss as

class LookupProxy a where
  lookupProxy :: RowProxy a -> Table b -> IO (Row a)

instance LookupProxy '[] where
  lookupProxy _ _ = return Nil

instance (KnownSymbol s, Storable b, LookupProxy bs) => LookupProxy ('(s, b) ': bs) where
  lookupProxy (ConsProxy _ offset as) table = do
    row <- withForeignPtr (tablePtr table) $ \p -> peek (p `plusPtr` offset)
    rest <- lookupProxy as $ unsafeCoerce table
    return (Cons (Proxy @s) row rest)

select ::
  forall as bs.
  ( LookupProxy (SelectFromT as bs),
    ToRowProxy (SelectFromT as bs),
    Storable (Row (SelectFromT as bs))
  ) =>
  Table bs ->
  IO [(Row (SelectFromT as bs))]
select = select' 0
  where
    select' offset table = do
      res <- lookup @as @bs offset table
      case res of
        Nothing -> return []
        Just row -> do
          rest <- select' (offset + sizeOf (undefined :: Row (SelectFromT as bs))) table
          return $ row : rest

lookup ::
  forall as bs.
  ( LookupProxy (SelectFromT as bs),
    ToRowProxy (SelectFromT as bs),
    Storable (Row (SelectFromT as bs))
  ) =>
  Int ->
  Table bs ->
  IO (Maybe (Row (SelectFromT as bs)))
lookup offset table = do
  let row = toRowProxy @(SelectFromT as bs) 0
  if tableLength table - offset >= (sizeOf (undefined :: Row (SelectFromT as bs)))
    then Just <$> lookupProxy row table
    else return Nothing
