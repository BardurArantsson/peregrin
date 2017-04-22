{-# LANGUAGE OverloadedStrings #-}
module Database.Peregrin.Metadata
    ( Identifier(..)
    , QualifiedIdentifier(..)
    , Schema(..)
    , Table(..)
    , ToSQL(..)
    , Typ(..)
    ) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Database.PostgreSQL.Simple.ToField (ToField(..))
import qualified Database.PostgreSQL.Simple.Types as PST

-- | An /unqalified/ identifier of an object in the database, i.e.
-- an identifier without an attached schema.
data Identifier = Identifier Text

instance ToField Identifier where
  toField (Identifier i) = toField $ PST.Identifier i

-- | A /qualified/ identifier of an object in the database, i.e.
-- an identifier with an attached schema.
data QualifiedIdentifier = QualifiedIdentifier Schema Text

instance ToField QualifiedIdentifier where
  toField (QualifiedIdentifier schema i) = toField $ PST.QualifiedIdentifier (Just $ schemaToText schema) i

-- | A schema designation.
data Schema = DefaultSchema
            | NamedSchema Text

instance ToField Schema where
  toField schema = toField $ Identifier $ schemaToText schema

-- | Convert schema to 'Text'.
schemaToText :: Schema -> Text
schemaToText DefaultSchema = "public"
schemaToText (NamedSchema schemaId) = schemaId

schemaToIdentifier :: Schema -> Identifier
schemaToIdentifier s = Identifier $ schemaToText s

-- | Table name, including which schema it is in.
data Table = Table Schema Text

instance ToField Table where
  toField (Table schema name) = toField $ QualifiedIdentifier schema name

-- | Type name, including which schema it is in.
data Typ = Typ Schema Text

instance ToField Typ where
  toField (Typ schema name) = toField $ QualifiedIdentifier schema name

-- | Convert metadata object identifier to its quoted SQL
-- representation.
class ToSQL a where
    toSQL :: a -> Text

--
-- Instances
--

instance ToSQL Schema where
  toSQL = toSQL . schemaToIdentifier

instance ToSQL Table where
  toSQL (Table s ti) = toSQL $ QualifiedIdentifier s ti

instance ToSQL Typ where
  toSQL (Typ schema tableId) = toSQL $ QualifiedIdentifier schema tableId

instance ToSQL Identifier where
  toSQL (Identifier i) =
    T.concat [ singleQt, T.replace singleQt doubleQt i , singleQt ]
    where
      singleQt = "\""
      doubleQt = "\"\""

instance ToSQL QualifiedIdentifier where
  toSQL (QualifiedIdentifier s i) =
    T.concat [toSQL s, ".", toSQL $ Identifier i]
