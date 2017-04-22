{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Peregrin
    ( migrate
    , MigrationError(..)
    , QP(..)
    ) where

import           Control.Applicative ((<$>))
import           Control.Exception (Exception, throwIO)
import           Control.Monad (forM_, when, void)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Int (Int32, Int64)
import           Data.Maybe (listToMaybe, fromMaybe)
import           Database.Peregrin.Metadata
import           Database.PostgreSQL.Simple (Connection, Only(..), formatQuery)
import qualified Database.PostgreSQL.Simple as P
import           Database.PostgreSQL.Simple.ToRow (ToRow(..))
import           Database.PostgreSQL.Simple.Types (Query(..))
import           Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import           Database.PostgreSQL.Simple.Transaction (withTransactionLevel, IsolationLevel(..))

-- | Migration information stored in 'migration' table.
data Migration = Migration Text ByteString

instance FromRow Migration where
  fromRow = Migration <$> field <*> field

-- | Exception happened running migrations.
data MigrationError =
    -- | The migration with the given ID has been modified in the
    -- program code since it was applied. Since this can have very
    -- unpredictable effects it is considered an error.
    MigrationModifiedError Text
  deriving (Show, Eq)

instance Exception MigrationError

-- | Context for migrations.
data MigrationContext = MigrationContext { mcMetaMigrationTable :: QIdentifier
                                         , mcMigrationTable :: QIdentifier
                                         }

-- | Parameter wrapper for a query. Used in when there are several
-- sets of parameters which must have the same type.
data QP = forall p . ToRow p => QP p

instance ToRow QP where
  toRow (QP qp) = toRow qp

-- | Apply a list of migrations to a database. For example,
--
-- > migrate conn schema [("a", "CREATE TABLE ...", QP $ Only $ Table schema "foo")]
-- >                     [("b", "INSERT INTO TABLE ...", QP $ (Table schema "foo", "bar"))]
--
-- will apply the given SQL statements __in order__ and track them by
-- the identifiers "a" and "b". It is recommended to use __fixed__,
-- randomly generated UUIDs to identify migrations, though you are
-- free to use whatever identifiers you like as long as they are
-- unique within the given schema. For example, on a Linux system you
-- can run the command `uuidgen -r` on the command line and paste that
-- into your migration list.
--
-- If the parameter sets are all of the same "shape" (type), then the
-- `P $` prefix may be omitted — it serves only to make sure that
-- the types match up.
--
-- The given 'Schema' parameter indicates the schema used for the
-- /metadata/ stored to track which migrations have been applied. It
-- does not affect the migrations themselves in any way. Therefore,
-- __ALL__ migrations should __ALWAYS__ specify their schema
-- explicitly in the SQL.
--
-- Any migrations that have already been applied will be skipped. If
-- the SQL text for any given migration /changes/, a
-- 'MigrationModifiedError' exception will be thrown.
--
-- Migrations are tracked using two tables, namely
-- "@\__peregrin_migration_meta\__@" and "@\__peregrin_migration\__@",
-- which will automatically be created in the given 'Schema'.
--
migrate :: ToRow p => Connection -> Schema -> [(Text, Query, p)] -> IO ()
migrate connection schema =
    migrate' tables connection schema
  where
    tables = MigrationContext { mcMetaMigrationTable = QIdentifier schema "__peregrin_migration_meta__"
                              , mcMigrationTable = QIdentifier schema "__peregrin_migration__"
                              }

migrate' :: ToRow p => MigrationContext -> Connection -> Schema -> [(Text, Query, p)] -> IO ()
migrate' tables c schema migrations = do
  -- Must always create the "migration_meta" table (and its
  -- schema) if necessary. Having just created this table without
  -- any rows represents "version 0" of the metadata data
  -- structures. These operations are idempotent and so we don't
  -- need any lock.
  void $ transact $ execute sqlCreateSchema (Only schema)
  void $ transact $ execute sqlCreateMetaTbl (Only metaTable)
  -- Apply meta-migrations.
  withLock $
    -- Apply meta-migrations; these are hardcoded for obvious reasons.
    -- EXCEPT for the very first migration, NO changes may be made to
    -- the "migration_meta" table in any migration here. This is to
    -- ensure 'perpetual' compatibility.
    metaMigrate 1 [ (sqlInsertMetaVersion0, (Only metaTable))
                  , (sqlCreateMigrationTbl, (Only migrationTable))
                  ]
  -- Apply all the migrations; we do it one-by-one since our lock is
  -- itself automatically released by PostgreSQL at the end of each of
  -- each transaction.
  forM_ migrations $ \(mid, q, p) ->
    withLock $ do
      -- Subsitute parameters
      sql <- formatQuery c q p
      -- Check if change set has already been applied
      existingMigration :: (Maybe Migration) <-
        listToMaybe <$> query sqlFindMigration ( migrationTable
                                               , mid
                                               )
      case existingMigration of
        Just (Migration _ sql') | sql == sql' ->
          return ()
        Just _ ->
          throwIO $ MigrationModifiedError mid
        Nothing -> do
          void $ execute sqlInsertMigration ( migrationTable
                                            , mid
                                            , sql
                                            )
          void $ execute_ $ Query sql

  where

    -- Tables
    migrationTable = mcMigrationTable tables
    metaTable = mcMetaMigrationTable tables

    -- Apply a meta-migration.
    metaMigrate :: ToRow a => Int32 -> [(Query, a)] -> IO ()
    metaMigrate metaVersion sqls = do
      -- Get the meta-version; defaults to 0 if we've only just
      -- created the metadata table.
      Only currentMetaVersion <- fromMaybe (Only 0) <$>
                                   fmap listToMaybe (query sqlGetMetaVersion $ Only metaTable)
      -- If the migration is applicable, then we apply it.
      when (currentMetaVersion + 1 == metaVersion) $ do
        forM_ sqls $ \(q, ps) -> execute q ps
        rowCount <- execute sqlUpdateMetaVersion ( metaTable
                                                 , metaVersion
                                                 , currentMetaVersion
                                                 )
        when (rowCount /= 1) $ error $ "Unexpected row count " ++ show rowCount ++ " from update on \"migration_meta\" table!"

    -- Shorthand:
    transact = withTransactionLevel ReadCommitted c

    execute :: ToRow a => Query -> a -> IO Int64
    execute = P.execute c

    execute_ :: Query -> IO Int64
    execute_ = P.execute_ c

    query :: (ToRow a, FromRow r) => Query -> a -> IO [r]
    query = P.query c

    -- Perform a transaction with the exclusive lock held. The lock is
    -- automatically released when the transaction ends.
    withLock txn =
      transact $ do
        void $ execute sqlLockMetaTbl [metaTable]
        txn

    -- Support SQL:
    sqlCreateSchema =
      "CREATE SCHEMA IF NOT EXISTS ?"

    sqlCreateMetaTbl =
      "CREATE TABLE IF NOT EXISTS ? (\
      \  \"meta_version\" INTEGER PRIMARY KEY\
      \)"

    sqlGetMetaVersion =
      "SELECT \"meta_version\" FROM ?"

    sqlUpdateMetaVersion =
      "UPDATE ? \
      \   SET \"meta_version\" = ? \
      \ WHERE \"meta_version\" = ?"

    sqlLockMetaTbl =
      "LOCK TABLE ? IN ACCESS EXCLUSIVE MODE"

    sqlInsertMetaVersion0 =
      "INSERT INTO ? (\"meta_version\") VALUES (0)"

    sqlCreateMigrationTbl =
      "CREATE TABLE ? ( \
      \  \"id\" TEXT PRIMARY KEY,\
      \  \"sql\" TEXT NOT NULL\
      \)"

    sqlFindMigration =
      "SELECT \"id\", \"sql\"\
      \ FROM ? \
      \ WHERE \"id\" = ?"

    sqlInsertMigration =
      "INSERT INTO ? \
       \           (\"id\", \"sql\") \
       \    VALUES (?, ?)"
