{-# LANGUAGE OverloadedStrings #-}
module Database.Peregrin.Metadata
    ( Identifier(..)
    , QualifiedIdentifier(..)
    , Schema(..)
    , Table(..)
    , Typ(..)
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

-- | Table name, including which schema it is in.
data Table = Table Schema Text

instance ToField Table where
  toField (Table schema name) = toField $ QualifiedIdentifier schema name

-- | Type name, including which schema it is in.
data Typ = Typ Schema Text

instance ToField Typ where
  toField (Typ schema name) = toField $ QualifiedIdentifier schema name
