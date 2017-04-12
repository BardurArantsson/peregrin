{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import           Control.Applicative ((<$>))
import           Database.PeregrinSpec ( mkMigrateSpec )
import           Data.Pool (createPool)
import qualified Database.PostgreSQL.Harness.Client as H
import           Database.PostgreSQL.Simple (connectPostgreSQL, close)
import           Test.Hspec

main :: IO ()
main = do
  -- HSpec has no easy way to get "other" command line parameters, so
  -- we'll just settle for a hardcoded value here.
  let url = "http://localhost:8900"
  -- Connection pool creation function. We use a fresh temporary
  -- database for every connection pool.
  let mkConnectionPool = do
        connectionString <- H.toConnectionString <$> H.createTemporaryDatabase url
        createPool (connectPostgreSQL connectionString) close 1 1 5
  -- Run
  hspec $ mkMigrateSpec mkConnectionPool
