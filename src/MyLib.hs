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
{-# LANGUAGE TypeOperators #-}

module MyLib where

import Data.Char
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Typeable
import Foreign
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.Parsec
import Text.Parsec.String

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
  show r = "[" ++ showRow r

class ShowRow a where
  showRow :: Row a -> String

instance ShowRow '[] where
  showRow Nil = "]"

instance (Show a) => ShowRow '[ '(s, a)] where
  showRow (Cons _ a Nil) = show a ++ "]"

instance (Show a, ShowRow (b ': bs)) => ShowRow ('(s, a) ': b ': bs) where
  showRow (Cons _ a as) = show a ++ ", " ++ showRow as

data Table (a :: [(Symbol, Type)]) = Table
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

data SomeRowProxy = forall a. (ShowRowProxy a) => SomeRowProxy (RowProxy a)

data SomeColumnProxy = forall s a. (KnownSymbol s, Typeable a) => SomeColumnProxy (Proxy ('(s, a)))

newtype TableSchema = TableSchema {unTableSchema :: Map String (SomeColumnProxy)}

class ToRowProxy (a :: [(Symbol, Type)]) where
  toRowProxy :: RowProxy a

instance ToRowProxy '[] where
  toRowProxy = NilProxy

class ToTableSchema (a :: [(Symbol, Type)]) where
  toTableSchema :: Int -> TableSchema

instance ToTableSchema '[] where
  toTableSchema _ = TableSchema Map.empty

instance (KnownSymbol s, Typeable a, Storable a, ToTableSchema as) => ToTableSchema ('(s, a) ': as) where
  toTableSchema i = case toTableSchema @as (i + 1) of
    TableSchema m -> TableSchema (Map.insert (symbolVal (Proxy @s)) (SomeColumnProxy (Proxy @('(s, a)))) m)

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

data SomeTable = forall a. (ShowRowProxy a, ToTableSchema a) => SomeTable (Table a)

newtype Database = Database {unDatabase :: Map String SomeTable}

database :: (ShowRowProxy a, ToTableSchema a) => String -> Table a -> Database
database ident t = Database (Map.singleton ident (SomeTable t))

newtype TableProxy a = TableProxy (RowProxy a)
  deriving (Show)

data RowProxy (as :: [(Symbol, Type)]) where
  NilProxy :: RowProxy '[]
  ConsProxy :: Proxy '(s, b) -> RowProxy bs -> RowProxy ('(s, b) ': bs)

instance (ShowRowProxy a) => Show (RowProxy a) where
  show _ = "(" ++ showRowProxy @a

class ShowRowProxy (a :: [(Symbol, Type)]) where
  showRowProxy :: String

instance ShowRowProxy '[] where
  showRowProxy = ")"

instance {-# OVERLAPPING #-} (KnownSymbol s, Typeable a) => ShowRowProxy '[ '(s, a)] where
  showRowProxy = symbolVal (Proxy @s) ++ " :: " ++ show (typeRep (Proxy @a)) ++ ")"

instance (KnownSymbol s, Typeable a, ShowRowProxy bs) => ShowRowProxy ('(s, a) ': bs) where
  showRowProxy = symbolVal (Proxy @s) ++ " :: " ++ show (typeRep (Proxy @a)) ++ rest
    where
      rest = case showRowProxy @bs of
        ")" -> ")"
        xs -> ", " ++ xs

class SelectFrom' (s :: Symbol) b a where
  selectFrom' :: Table a -> b

class SelectFrom b a where
  selectFrom :: Table a -> Row b

instance SelectFrom '[] a where
  selectFrom _ = Nil

instance (KnownSymbol s, SelectFrom' s b as, SelectFrom bs as) => SelectFrom ('(s, b) ': bs) as where
  selectFrom table = Cons (Proxy @s) (selectFrom' @s @b @as table) (selectFrom table)

data SelectExpr a (b :: [(Symbol, Type)]) = SelectExpr
  { selectExprFields :: RowProxy b,
    selectExprfromTable :: Proxy (Table a)
  }

instance (ShowRowProxy a, ShowRowProxy b) => Show (SelectExpr a b) where
  show (SelectExpr fields _) = "SELECT " ++ show fields ++ " FROM (" ++ showRowProxy @a

data SomeSelectExpr = forall a b. (Show (SelectExpr a b)) => SomeSelectExpr (SelectExpr a b)

instance Show SomeSelectExpr where
  show (SomeSelectExpr s) = show s

tableProxy :: Table a -> Proxy (Table a)
tableProxy _ = Proxy

tableToSchema :: forall a. (ToTableSchema a) => Table a -> TableSchema
tableToSchema _ = toTableSchema @a 0

compile :: SQLSelect -> Database -> SomeSelectExpr
compile sqlSelect db = case sqlSelect of
  SQLSelect fields table -> case Map.lookup table (unDatabase db) of
    Just (SomeTable t) ->
      let schema = tableToSchema t
          fields' =
            foldr
              ( \ident (SomeRowProxy acc) -> case Map.lookup ident (unTableSchema schema) of
                  Just (SomeColumnProxy a) -> SomeRowProxy (ConsProxy a acc)
                  Nothing -> error "TODO"
              )
              (SomeRowProxy NilProxy)
              fields
       in case fields' of
            SomeRowProxy a -> SomeSelectExpr $ SelectExpr a (tableProxy t)
    Nothing -> error "TODO"

run :: String -> Database -> IO ()
run s db = case parse sqlSelectParser "" s of
  Left e -> print e
  Right sqlSelect -> case compile sqlSelect db of
    SomeSelectExpr selectExpr -> print selectExpr
