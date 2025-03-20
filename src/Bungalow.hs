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

module Bungalow where

import Bungalow.Row
import Bungalow.Table
import Data.Char
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Typeable
import Foreign
import GHC.TypeLits
import Text.Parsec
import Text.Parsec.String
import Unsafe.Coerce
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

class ToRowProxy (a :: [(Symbol, Type)]) where
  toRowProxy :: Int -> RowProxy a

instance ToRowProxy '[] where
  toRowProxy _ = NilProxy

instance (KnownSymbol s, Storable a, ToRowProxy as) => ToRowProxy ('(s, a) ': as) where
  toRowProxy offset =
    ConsProxy (Proxy @'(s, a)) offset (toRowProxy @as $ offset + sizeOf (undefined :: a))

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
  { selectFields :: [String],
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

newtype Database = Database {unDatabase :: Map String SomeTable}

database ::
  (ShowRow a, ShowRowProxy a, ToRowProxy a, ToTableSchema a, Storable (Row a)) =>
  String ->
  Table a ->
  Database
database ident t = Database (Map.singleton ident (SomeTable t))

data TableProxy a = TableProxy (RowProxy a) String
  deriving (Show)

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

type family SelectFromT' (s :: Symbol) (as :: [(Symbol, Type)]) where
  SelectFromT' s ('(s, a) ': xs) = a
  SelectFromT' s ('(t, a) ': xs) = SelectFromT' s xs

type family (++) (as :: [(Symbol, Type)]) (bs :: [(Symbol, Type)]) where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

type family SelectFromT (s :: [Symbol]) (as :: [(Symbol, Type)]) where
  SelectFromT '[] as = '[]
  SelectFromT (s ': ss) as = '(s, SelectFromT' s as) ': SelectFromT ss as

select ::
  forall as bs.
  (LookupProxy (SelectFromT as bs), ToRowProxy (SelectFromT as bs)) =>
  Table bs ->
  IO (Row (SelectFromT as bs))
select table = do
  let row = toRowProxy @(SelectFromT as bs) 0
  lookupProxy row table

data SelectExpr a (b :: [(Symbol, Type)]) = SelectExpr
  { selectExprFields :: RowProxy b,
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

compile :: SQLSelect -> Database -> SomeSelectExpr
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

class LookupProxy a where
  lookupProxy :: RowProxy a -> Table b -> IO (Row a)

instance LookupProxy '[] where
  lookupProxy _ _ = return Nil

instance (KnownSymbol s, Storable b, LookupProxy bs) => LookupProxy ('(s, b) ': bs) where
  lookupProxy (ConsProxy _ offset as) table = do
    row <- withForeignPtr (tablePtr table) $ \p -> peek (p `plusPtr` offset)
    rest <- lookupProxy as $ unsafeCoerce table
    return (Cons (Proxy @s) row rest)

run :: String -> Database -> IO ()
run s db = case parse sqlSelectParser "" s of
  Left e -> print e
  Right sqlSelect -> case compile sqlSelect db of
    SomeSelectExpr selectExpr -> do
      print selectExpr
      let (TableProxy _ tableIdent) = selectExprfromTable selectExpr
      case Map.lookup tableIdent (unDatabase db) of
        Just (SomeTable t) -> do
          x <- lookupProxy (selectExprFields selectExpr) t
          print x
        Nothing -> error ""
