{-# LANGUAGE OverloadedStrings #-}
module Database.Peregrin.Metadata
    ( Schema(..)
    , Table(..)
    , ToSQL(..)
    , Typ(..)
    ) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Database.PostgreSQL.Simple.ToField (ToField(..))
import           Database.PostgreSQL.Simple.Types (Identifier(..), QualifiedIdentifier(..))

-- | A schema designation.
data Schema = DefaultSchema
            | NamedSchema Text

instance ToField Schema where
  toField schema = toField $ Identifier $ schemaToText schema

-- | Convert schema to 'Text'.
schemaToText :: Schema -> Text
schemaToText DefaultSchema = "public"
schemaToText (NamedSchema schemaId) = schemaId

-- | Create a qualified identifier.
mkQualifiedIdentifier :: Schema -> Text -> QualifiedIdentifier
mkQualifiedIdentifier schema = QualifiedIdentifier (Just $ schemaToText schema)

-- | Table name, including which schema it is in.
data Table = Table Schema Text

instance ToField Table where
  toField (Table schema name) = toField $ mkQualifiedIdentifier schema name

-- | Type name, including which schema it is in.
data Typ = Typ Schema Text

instance ToField Typ where
  toField (Typ schema name) = toField $ mkQualifiedIdentifier schema name

-- | Quote a PostgreSQL object identifier. Useful in circumstances
-- where you need to quote a dynamically generated identifier.
quoteToSQL :: Text -> Text
quoteToSQL i = T.concat [ singleQt
                        , T.replace singleQt doubleQt i
                        , singleQt
                        ]
  where
    singleQt = "\""
    doubleQt = "\"\""

-- | Convert metadata object identifier to its quoted SQL
-- representation.
class ToSQL a where
    toSQL :: a -> Text

--
-- Instances
--

instance ToSQL Schema where
  toSQL DefaultSchema = quoteToSQL "public"
  toSQL (NamedSchema schemaId) = quoteToSQL schemaId

instance ToSQL Table where
  toSQL (Table schema tableId) =
    T.concat [toSQL schema, ".", quoteToSQL tableId]

instance ToSQL Typ where
  toSQL (Typ schema tableId) =
    T.concat [toSQL schema, ".", quoteToSQL tableId]
