{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import           Control.Applicative ((<$>))
import           Database.PeregrinSpec ( mkMigrateSpec )
import           Data.Maybe (fromMaybe)
import           Data.Pool (newPool, defaultPoolConfig)
import qualified Database.PostgreSQL.Harness.Client as H
import           Database.PostgreSQL.Simple (connectPostgreSQL, close)
import           System.Environment (lookupEnv)
import           Test.Hspec

main :: IO ()
main = do
  -- We require an environment variable to point to a tempgres server instance.
  url <- fmap
    (fromMaybe (error "Missing TEMPGRES_URL environment variable"))
    (lookupEnv "TEMPGRES_URL")
  -- Connection pool creation function. We use a fresh temporary
  -- database for every connection pool.
  let mkConnectionPool = do
        connectionString <- H.toConnectionString <$> H.createTemporaryDatabase url
        let poolConfig = defaultPoolConfig (connectPostgreSQL connectionString) close 1.0 5
        newPool poolConfig
  -- Run
  hspec $ mkMigrateSpec mkConnectionPool
