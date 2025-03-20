{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Bungalow.Row where

import Data.Kind
import Data.Proxy
import Data.Typeable
import Foreign
import GHC.TypeLits

data Row as where
  Nil :: Row '[]
  Cons :: (KnownSymbol s) => Proxy s -> b -> Row bs -> Row ('(s, b) ': bs)

instance Storable (Row '[]) where
  sizeOf _ = 0
  alignment _ = 1
  peek _ = return Nil
  poke _ Nil = return ()

instance (KnownSymbol s, Storable a, Storable (Row as)) => Storable (Row ('(s, a) ': as)) where
  sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: Row as)
  alignment _ = alignment (undefined :: a) `max` alignment (undefined :: Row as)
  peek p = do
    a <- peek (castPtr p)
    as <- peek (castPtr (p `plusPtr` sizeOf a))
    return (Cons (Proxy @s) a as)
  poke p (Cons _ a as) = do
    poke (castPtr p) a
    poke (castPtr (p `plusPtr` sizeOf a)) as

instance (ShowRow a) => Show (Row a) where
  show r = "(" ++ showRow r

class ShowRow a where
  showRow :: Row a -> String

instance ShowRow '[] where
  showRow Nil = ")"

instance {-# OVERLAPPING #-} (Show a) => ShowRow '[ '(s, a)] where
  showRow (Cons _ a Nil) = show a ++ ")"

instance (Show a, ShowRow bs) => ShowRow ('(s, a) ': bs) where
  showRow (Cons _ a as) = show a ++ rest
    where
      rest = case showRow as of
        ")" -> ")"
        xs -> ", " ++ xs

class ToRow bs a where
  toRow :: a -> Row bs

infixr 5 :&

data a :& b = a :& b
  deriving (Show)

instance ToRow bs (Row bs) where
  toRow = id

instance (KnownSymbol s) => ToRow '[ '(s, a)] a where
  toRow a = Cons (Proxy @s) a Nil

instance (KnownSymbol s, ToRow bs as) => ToRow ('(s, a) ': bs) (a :& as) where
  toRow (a :& as) = Cons (Proxy @s) a (toRow @bs as)

data RowProxy (as :: [(Symbol, Type)]) where
  NilProxy :: RowProxy '[]
  ConsProxy :: Proxy '(s, b) -> Int -> RowProxy bs -> RowProxy ('(s, b) ': bs)

instance (ShowRowProxy a) => Show (RowProxy a) where
  show p = "(" ++ showRowProxy p

class ShowRowProxy (a :: [(Symbol, Type)]) where
  showRowProxy :: RowProxy a -> String

instance ShowRowProxy '[] where
  showRowProxy _ = ")"

instance {-# OVERLAPPING #-} (KnownSymbol s, Typeable a) => ShowRowProxy '[ '(s, a)] where
  showRowProxy (ConsProxy _ offset NilProxy) =
    symbolVal (Proxy @s) ++ " :: " ++ show offset ++ " :: " ++ show (typeRep (Proxy @a)) ++ ")"

instance (KnownSymbol s, Typeable a, ShowRowProxy as) => ShowRowProxy ('(s, a) ': as) where
  showRowProxy (ConsProxy _ offset as) =
    symbolVal (Proxy @s) ++ " :: " ++ show offset ++ " :: " ++ show (typeRep (Proxy @a)) ++ rest
    where
      rest = case showRowProxy as of
        ")" -> ")"
        xs -> ", " ++ xs

class ToRowProxy (a :: [(Symbol, Type)]) where
  toRowProxy :: Int -> RowProxy a

instance ToRowProxy '[] where
  toRowProxy _ = NilProxy

instance (KnownSymbol s, Storable a, ToRowProxy as) => ToRowProxy ('(s, a) ': as) where
  toRowProxy offset =
    ConsProxy (Proxy @'(s, a)) offset (toRowProxy @as $ offset + sizeOf (undefined :: a))
