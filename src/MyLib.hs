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
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (lookup)

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

lookup :: forall a b. (Storable (Row a)) => Int -> Table b -> IO (Row a)
lookup i table = withForeignPtr (tablePtr table) $ \p -> do
  peek (p `plusPtr` (i * sizeOf (undefined :: Row a)))

data SomeRowProxy = forall a. (ShowRow a, ShowRowProxy a, Storable (Row a), LookupProxy a) => SomeRowProxy (RowProxy a)

data SomeColumnProxy = forall s a. (KnownSymbol s, Typeable a, Storable a, Show a) => SomeColumnProxy (Proxy ('(s, a))) Int

newtype TableSchema = TableSchema {unTableSchema :: Map String (SomeColumnProxy)}

class ToRowProxy (a :: [(Symbol, Type)]) where
  toRowProxy :: Int -> RowProxy a

instance ToRowProxy '[] where
  toRowProxy _ = NilProxy

instance (KnownSymbol s, Storable a, ToRowProxy as) => ToRowProxy ('(s, a) ': as) where
  toRowProxy offset = ConsProxy (Proxy @'(s, a)) offset (toRowProxy @as $ offset + sizeOf (undefined :: a))

class ToTableSchema (a :: [(Symbol, Type)]) where
  toTableSchema :: Int -> TableSchema

instance ToTableSchema '[] where
  toTableSchema _ = TableSchema Map.empty

instance (KnownSymbol s, Typeable a, Storable a, Show a, ToTableSchema as) => ToTableSchema ('(s, a) ': as) where
  toTableSchema i = case toTableSchema @as (i + sizeOf (undefined :: a)) of
    TableSchema m -> TableSchema (Map.insert (symbolVal (Proxy @s)) (SomeColumnProxy (Proxy @('(s, a))) i) m)

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

data SomeTable = forall a. (ShowRow a, ShowRowProxy a, ToRowProxy a, ToTableSchema a, Storable (Row a)) => SomeTable (Table a)

newtype Database = Database {unDatabase :: Map String SomeTable}

database :: (ShowRow a, ShowRowProxy a, ToRowProxy a, ToTableSchema a, Storable (Row a)) => String -> Table a -> Database
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
  showRowProxy (ConsProxy _ offset NilProxy) = symbolVal (Proxy @s) ++ " :: " ++ show offset ++ " :: " ++ show (typeRep (Proxy @a)) ++ ")"

instance (KnownSymbol s, Typeable a, ShowRowProxy as) => ShowRowProxy ('(s, a) ': as) where
  showRowProxy (ConsProxy _ offset as) = symbolVal (Proxy @s) ++ " :: " ++ show offset ++ " :: " ++ show (typeRep (Proxy @a)) ++ rest
    where
      rest = case showRowProxy as of
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
    selectExprfromTable :: TableProxy a
  }

instance (ShowRowProxy a, ShowRowProxy b) => Show (SelectExpr a b) where
  show (SelectExpr fields t) = "SELECT " ++ show fields ++ " FROM (" ++ show t

data SomeSelectExpr = forall a b. (Show (SelectExpr a b), Storable (Row b), Show (Row b), LookupProxy b) => SomeSelectExpr (SelectExpr a b)

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
