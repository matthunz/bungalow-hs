{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

    -- * Access
    Access (..),
    runAccess,
    runAccess_,
    runSql,

    -- * Row
    (:&) (..),

    -- * Database
    Schema,
    col,
    newDatabase,
  )
where

import Bungalow.Database hiding (insert, select, selectColumns)
import qualified Bungalow.Database as DB
import Bungalow.Row
import Bungalow.Table hiding (insert, select)
import Control.Monad.State
import Data.Kind
import Data.Proxy
import Foreign
import GHC.TypeLits

class Eval bs a where
  type EvalT (bs :: [Type]) a :: Type
  eval :: a -> Access bs (EvalT bs a)

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

  eval s = Access $ do
    db <- get
    liftIO $ DB.select @s @as @bs (selectColumns s) (selectTable s) db

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
  type EvalT bs (Insert s as) = ()

  eval i = Access $ do
    db <- get
    db' <- liftIO $ DB.insert (insertTable i) (insertValues i) db
    put db'

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

newtype Access db a = Access {unAccess :: StateT (Database db) IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runAccess_ :: (NewDatabase db) => Access db a -> IO a
runAccess_ access = do
  db <- newDatabase
  (a, _) <- runAccess access db
  return a

runAccess :: Access db a -> Database db -> IO (a, Database db)
runAccess = runStateT . unAccess

runSql :: String -> Access db ()
runSql sql = Access $ do
  db <- get
  liftIO $ DB.run sql db
