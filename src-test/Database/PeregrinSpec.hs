{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.PeregrinSpec
    ( mkMigrateSpec
    ) where

import           Control.Exception (Exception, bracket)
import           Control.Monad (forM_)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Database.Peregrin
import           Database.Peregrin.Metadata
import           Data.Pool (Pool, withResource, destroyAllResources)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection, Query, Only(..), FromRow)
import qualified Database.PostgreSQL.Simple as PS
import           Test.Hspec (Spec, Selector, describe)
import qualified Test.Hspec as Hspec

-- | Create specs.
mkMigrateSpec :: IO (Pool Connection) -> Spec
mkMigrateSpec mkConnectionPool =
  forM_ [DefaultSchema, NamedSchema "foobar"] $ \schema -> do
    -- Show which case we're in
    let extra = case schema of
                  DefaultSchema -> " (default schema)"
                  NamedSchema _ -> " (named schema)"
    -- Specs:
    describe ("migrate" ++ extra) $ do
      it "can apply a single migration" $ do
        -- Exercise:
        migrate' schema [ (cid0, createXSql) ]
        -- Verify:
        assertCanSelectFromX

      it "ignores migrations that have already been applied (single call)" $ do
        -- Exercise:
        migrate' schema [ (cid0, createXSql)
                        , (cid0, createXSql) -- Would fail if applied again
                        ]
        -- Verify:
        assertCanSelectFromX

      it "ignores migrations that have already been applied (multiple calls)" $ do
        -- Exercise:
        migrate' schema [ (cid0, createXSql) ]
        migrate' schema [ (cid0, createXSql) ] -- Would fail if applied again
        -- Verify:
        assertCanSelectFromX

      it "throws an error if SQL is changed for a given change set ID" $ do
        -- Exercise:
        migrate' schema [ (cid0, createXSql) ]
        migrate' schema [ (cid0, createXSqlBad) ]
          -- Verify: Should throw here
          `shouldThrow` (== MigrationModifiedError cid0)

      it "can apply multiple distinct migrations in a single call" $ do
        -- Exercise
        migrate' schema [ (cid0, createXSql)
                        , (cid1, createYSql)
                        ]
        -- Verify: Make sure both migrations have been applied
        assertCanSelectFromXY

  where
    it msg action = Hspec.it msg $
      bracket mkConnectionPool destroyAllResources $ runReaderT action

    createXSql = "CREATE TABLE X (A INT)"
    createYSql = "CREATE TABLE Y (B INT)"
    createXSqlBad = "CREATE TABLE X (Y CHAR(1))"

    assertCanSelectFromX =
      assertCanQuery_ "SELECT * FROM X"

    assertCanSelectFromXY =
      assertCanQuery_ "SELECT * FROM X, Y where X.A = Y.B"

    cid0 = "a328156d-9875-4471-8192-0c86959badb3"
    cid1 = "00c6159c-c7f6-4cec-b63f-f70c1c4c7bb1"

assertCanQuery_ :: Query -> ReaderT (Pool Connection) IO ()
assertCanQuery_ q = do
  _ :: [Only Int] <- query_ q
  return ()

query_ :: FromRow a => Query -> ReaderT (Pool Connection) IO [a]
query_ sql = do
  connectionPool <- ask
  withResource connectionPool $ \connection ->
    lift $ PS.query_ connection sql

shouldThrow :: Exception e => ReaderT (Pool Connection) IO a -> Selector e -> ReaderT (Pool Connection) IO ()
shouldThrow action selector = do
  connectionPool <- ask
  lift (Hspec.shouldThrow (runReaderT action connectionPool) selector)

migrate' :: Schema -> [(Text, Text)] -> ReaderT (Pool Connection) IO ()
migrate' schema migrations = do
  connectionPool <- ask
  lift $ withResource connectionPool $ \connection ->
    migrate connection schema migrations
