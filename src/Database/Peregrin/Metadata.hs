{-# LANGUAGE OverloadedStrings #-}
module Database.Peregrin.Metadata
    ( Identifier(..)
    , QIdentifier(..)
    , Schema(..)
    ) where

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
data QIdentifier = QIdentifier Schema Text

instance ToField QIdentifier where
  toField (QIdentifier schema i) = toField $ PST.QualifiedIdentifier (Just $ schemaToText schema) i

-- | A schema designation.
data Schema = DefaultSchema
            | NamedSchema Text

instance ToField Schema where
  toField schema = toField $ Identifier $ schemaToText schema

-- | Convert schema to 'Text'.
schemaToText :: Schema -> Text
schemaToText DefaultSchema = "public"
schemaToText (NamedSchema schemaId) = schemaId
