{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bungalow.Database where

import Bungalow.Row
import Bungalow.Table
import qualified Bungalow.Table as Table
import Data.Char
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Typeable
import Foreign
import GHC.OverloadedLabels
import GHC.TypeLits
import Text.Parsec hiding (Column)
import Text.Parsec.String
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (lookup)

data SomeRowProxy
  = forall a.
    (ShowRow a, ShowRowProxy a, Storable (Row a), LookupProxy a) =>
    SomeRowProxy (RowProxy a)

data SomeColumnProxy
  = forall s a.
    (KnownSymbol s, Typeable a, Storable a, Show a) =>
    SomeColumnProxy (Proxy ('(s, a))) Int

newtype TableSchema = TableSchema {unTableSchema :: Map String (SomeColumnProxy)}

class ToTableSchema (a :: [(Symbol, Type)]) where
  toTableSchema :: Int -> TableSchema

instance ToTableSchema '[] where
  toTableSchema _ = TableSchema Map.empty

instance
  (KnownSymbol s, Typeable a, Storable a, Show a, ToTableSchema as) =>
  ToTableSchema ('(s, a) ': as)
  where
  toTableSchema i = case toTableSchema @as (i + sizeOf (undefined :: a)) of
    TableSchema m ->
      TableSchema (Map.insert (symbolVal (Proxy @s)) (SomeColumnProxy (Proxy @('(s, a))) i) m)

data SQLSelect = SQLSelect
  { selectColumns :: [String],
    fromTable :: String
  }
  deriving (Show)

sqlSelectParser :: Parser SQLSelect
sqlSelectParser = do
  spaces
  _ <- stringCI "select"
  spaces1
  fields <- sepBy1 identifier (char ',' >> spaces)
  spaces1
  _ <- stringCI "from"
  spaces1
  table <- identifier
  spaces
  eof
  return $ SQLSelect fields table

stringCI :: String -> Parser String
stringCI = try . mapM charCI

charCI :: Char -> Parser Char
charCI c = char (toLower c) <|> char (toUpper c)

identifier :: Parser String
identifier = do
  first <- letter <|> char '_'
  rest <- many (alphaNum <|> char '_')
  return (first : rest)

spaces1 :: Parser ()
spaces1 = skipMany1 space

data SomeTable
  = forall a.
    (ShowRow a, ShowRowProxy a, ToRowProxy a, ToTableSchema a, Storable (Row a)) =>
    SomeTable (Table a)

data Schema (s :: Symbol) (a :: [(Symbol, Type)])

class NewDatabase (as :: [Type]) where
  newDatabase :: IO (Database as)

instance NewDatabase '[] where
  newDatabase = return $ Database Map.empty

instance
  ( KnownSymbol s,
    Storable (Row a),
    ShowRow a,
    ShowRowProxy a,
    ToRowProxy a,
    ToTableSchema a,
    NewDatabase as
  ) =>
  NewDatabase (Schema s a ': as)
  where
  newDatabase = do
    t <- newTable @a
    db <- newDatabase @as
    return $ (Database (Map.singleton (symbolVal $ Proxy @s) (SomeTable t)) <> unsafeCoerce db)

newtype Database (as :: [(Type)]) = Database {unDatabase :: Map String SomeTable}
  deriving (Semigroup, Monoid)

type family HasTableT' (s :: Symbol) (as :: [Type]) :: Bool where
  HasTableT' s '[] = 'False
  HasTableT' s (Schema s a ': as) = 'True
  HasTableT' s (Schema t a ': as) = HasTableT' s as

type family HasTableT (s :: Symbol) (as :: [Type]) :: [(Symbol, Type)] where
  HasTableT s '[] = '[]
  HasTableT s (Schema s a ': as) = a
  HasTableT s (Schema t a ': as) = HasTableT s as

class HasTable' (flag :: Bool) (s :: Symbol) (as :: [(Type)]) where
  getTable' :: Database as -> Table (HasTableT s as)
  withTable' :: (Table (HasTableT s as) -> IO (Table (HasTableT s as))) -> Database as -> IO (Database as)

instance
  ( KnownSymbol s,
    ShowRow a,
    ShowRowProxy a,
    ToRowProxy a,
    ToTableSchema a,
    Storable (Row a)
  ) =>
  HasTable' True s (Schema s a ': as)
  where
  getTable' (Database db) = case Map.lookup (symbolVal $ Proxy @s) db of
    Just (SomeTable t) -> unsafeCoerce t
    Nothing -> error "TODO"
  withTable' f (Database db) = case Map.lookup (symbolVal $ Proxy @s) db of
    Just (SomeTable t) -> do
      b <- f (unsafeCoerce t)
      return $ Database (Map.insert (symbolVal $ Proxy @s) (SomeTable b) db)
    Nothing -> error "TODO"

instance (HasTable' (HasTableT' s as) s as) => HasTable' False s (Schema t a ': as) where
  getTable' (Database db) = unsafeCoerce $ getTable' @(HasTableT' s as) @s @as $ Database db
  withTable' f db = unsafeCoerce $ withTable' @(HasTableT' s as) @s @as (unsafeCoerce f) $ unsafeCoerce db

class HasTable (s :: Symbol) (as :: [(Type)]) where
  getTable :: Database as -> Table (HasTableT s as)
  withTable :: (Table (HasTableT s as) -> IO (Table (HasTableT s as))) -> Database as -> IO (Database as)

instance (HasTable' (HasTableT' s as) s as) => HasTable s as where
  getTable = getTable' @(HasTableT' s as) @s @as
  withTable = withTable' @(HasTableT' s as) @s @as

data Alias (alias :: Symbol) = Alias deriving (Eq, Ord, Show)

aliasVal :: forall s. (KnownSymbol s) => Alias s -> String
aliasVal _ = symbolVal (Proxy @s)

instance (a ~ b) => IsLabel a (Alias b) where
  fromLabel = Alias

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

newtype Column a = Column (Alias a)

col :: forall s. (KnownSymbol s) => Alias s -> Column s
col = Column

type family Selectable (a :: Type) :: [Symbol] where
  Selectable (Column s) = '[s]
  Selectable (a :& b) = Selectable a ++ Selectable b

select ::
  forall s as bs.
  ( HasTable s bs,
    LookupProxy (SelectFromT (Selectable as) (HasTableT s bs)),
    ToRowProxy (SelectFromT (Selectable as) (HasTableT s bs))
  ) =>
  as ->
  Alias s ->
  Database bs ->
  IO (Row (SelectFromT (Selectable as) (HasTableT s bs)))
select _ _ db = Table.select @(Selectable as) @(HasTableT s bs) $ getTable @s db

insert ::
  forall s as bs.
  (HasTable s bs, ToRow (HasTableT s bs) as, Storable (Row (HasTableT s bs))) =>
  Alias s ->
  as ->
  Database bs ->
  IO (Database bs)
insert _ as db =
  withTable @s (\t -> Table.insert @as @(HasTableT s bs) as t) db

data TableProxy a = TableProxy (RowProxy a) String
  deriving (Show)

data SelectExpr a (b :: [(Symbol, Type)]) = SelectExpr
  { selectExprColumns :: RowProxy b,
    selectExprfromTable :: TableProxy a
  }

instance (ShowRowProxy a, ShowRowProxy b) => Show (SelectExpr a b) where
  show (SelectExpr fields t) = "SELECT " ++ show fields ++ " FROM (" ++ show t

data SomeSelectExpr
  = forall a b.
    (Show (SelectExpr a b), Storable (Row b), Show (Row b), LookupProxy b) =>
    SomeSelectExpr (SelectExpr a b)

instance Show SomeSelectExpr where
  show (SomeSelectExpr s) = show s

tableToSchema :: forall a. (ToTableSchema a) => Table a -> TableSchema
tableToSchema _ = toTableSchema @a 0

tableToRowProxy :: forall a. (ToRowProxy a) => String -> Table a -> TableProxy a
tableToRowProxy ident _ = TableProxy (toRowProxy @a 0) ident

compile :: SQLSelect -> Database as -> SomeSelectExpr
compile sqlSelect db = case sqlSelect of
  SQLSelect fields table -> case Map.lookup table (unDatabase db) of
    Just (SomeTable t) ->
      let schema = tableToSchema t
          fields' =
            foldr
              ( \ident (SomeRowProxy acc) -> case Map.lookup ident (unTableSchema schema) of
                  Just (SomeColumnProxy a offset) -> SomeRowProxy (ConsProxy a offset acc)
                  Nothing -> error "TODO"
              )
              (SomeRowProxy NilProxy)
              fields
       in case fields' of
            SomeRowProxy a -> SomeSelectExpr $ SelectExpr a (tableToRowProxy table t)
    Nothing -> error "TODO"

run :: String -> Database as -> IO ()
run s db = case parse sqlSelectParser "" s of
  Left e -> print e
  Right sqlSelect -> case compile sqlSelect db of
    SomeSelectExpr selectExpr -> do
      print selectExpr
      let (TableProxy _ tableIdent) = selectExprfromTable selectExpr
      case Map.lookup tableIdent (unDatabase db) of
        Just (SomeTable t) -> do
          x <- lookupProxy (selectExprColumns selectExpr) t
          print x
        Nothing -> error ""
