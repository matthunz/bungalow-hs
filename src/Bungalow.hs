{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bungalow
  ( Eval (..),
    ToSql (..),
    Select (..),
    select,
    SelectColumnsToSql (..),
    Insert (..),
    insert,
    InsertValuesToSql (..),

    -- * Row
    (:&)(..),

    -- * Database
    Schema,
    field,
    newDatabase,
    run,
  )
where

import Bungalow.Database hiding (insert, select)
import qualified Bungalow.Database as DB
import Bungalow.Row
import Bungalow.Table hiding (insert, select)
import Data.Kind
import Data.Proxy
import Foreign
import GHC.TypeLits

class Eval bs a where
  type EvalT (bs :: [Type]) a :: Type
  eval :: a -> Database bs -> IO (EvalT bs a)

class ToSql (db :: [Type]) a where
  toSql :: a -> String

instance ToSql db Int32 where
  toSql = show

data Select as s = Select
  { selectColumns :: as,
    selectTable :: Alias s
  }

select :: as -> Alias s -> Select as s
select = Select

instance
  ( HasTable s bs,
    LookupProxy (SelectFromT (Selectable as) (HasTableT s bs)),
    ToRowProxy (SelectFromT (Selectable as) (HasTableT s bs))
  ) =>
  Eval bs (Select as s)
  where
  type EvalT bs (Select as s) = Row (SelectFromT (Selectable as) (HasTableT s bs))

  eval s = DB.select @s @as @bs (selectColumns s) (selectTable s)

instance
  (KnownSymbol s, SelectColumnsToSql (SelectFromT (Selectable as) (HasTableT s bs))) =>
  ToSql db (Select as s)
  where
  toSql (Select _ s) =
    "SELECT " ++ selectColumnsToSql @(SelectFromT (Selectable as) (HasTableT s bs)) ++ " FROM " ++ aliasVal s

class SelectColumnsToSql (as :: [(Symbol, Type)]) where
  selectColumnsToSql :: String

instance SelectColumnsToSql '[] where
  selectColumnsToSql = ""

instance (KnownSymbol s, SelectColumnsToSql as) => SelectColumnsToSql ('(s, a) ': as) where
  selectColumnsToSql =
    symbolVal (Proxy @s) ++ case selectColumnsToSql @as of
      "" -> ""
      xs -> ", " ++ xs

data Insert s as = Insert
  { insertTable :: Alias s,
    insertValues :: as
  }

insert :: Alias s -> as -> Insert s as
insert = Insert

instance
  (HasTable s bs, ToRow (HasTableT s bs) as, Storable (Row (HasTableT s bs))) =>
  Eval bs (Insert s as)
  where
  type EvalT bs (Insert s as) = Database bs

  eval i db = DB.insert (insertTable i) (insertValues i) db

instance (KnownSymbol s, ToRow (HasTableT s bs) as, InsertValuesToSql (HasTableT s bs)) => ToSql bs (Insert s as) where
  toSql (Insert s as) =
    "INSERT INTO " ++ aliasVal s ++ " VALUES (" ++ insertValuesToSql (toRow @(HasTableT s bs) as) ++ ")"

class InsertValuesToSql (as :: [(Symbol, Type)]) where
  insertValuesToSql :: Row as -> String

instance InsertValuesToSql '[] where
  insertValuesToSql _ = ""

instance (ToSql db a, InsertValuesToSql as) => InsertValuesToSql ('(s, a) ': as) where
  insertValuesToSql (Cons _ a as) =
    toSql @db a ++ case insertValuesToSql @as as of
      "" -> ""
      xs -> ", " ++ xs
