{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | End to end tests against a real SurrealDB server. These run only when a
-- @surreal@ binary is available; see "Test.Surreal.Harness".
module Test.Surreal.IntegrationSpec (tests) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           System.Timeout         (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Database.Surreal
import           Test.Surreal.Harness

data IPerson = IPerson
  { name :: Text
  , age  :: Int
  } deriving (Eq, Show, Generic)

instance SurrealRecord IPerson

tests :: IO Server -> TestTree
tests getServer = testGroup "Integration"
  [ testCase "websocket cbor crud" $ do
      srv <- getServer
      db  <- connect (wsUrl srv) defaultConnectOpts
      runSurreal db $ do
        _ <- signin (Root "root" "root")
        use (Just "ns1") (Just "db1")
        created <- create (onTable "person") (IPerson "Alice" 30) :: SurrealT IO [IPerson]
        liftIO (length created @?= 1)
        rows <- select (onTable "person") :: SurrealT IO [IPerson]
        liftIO (map name rows @?= ["Alice"])
        qr <- queryFirst "SELECT * FROM person WHERE age > $min" (Map.fromList [("min", VInt 18)]) :: SurrealT IO [IPerson]
        liftIO (length qr @?= 1)
        _ <- delete (onTable "person") :: SurrealT IO [IPerson]
        remaining <- select (onTable "person") :: SurrealT IO [IPerson]
        liftIO (remaining @?= [])
      close db

  , testCase "http json signin and version" $ do
      srv <- getServer
      db  <- connect (httpUrl srv) defaultConnectOpts { coCodec = CodecJson }
      runSurreal db $ do
        _ <- signin (Root "root" "root")
        use (Just "ns2") (Just "db2")
        v <- version
        liftIO (assertBool "server version is non empty" (not (T.null v)))
      close db

  , testCase "websocket live query receives a create" $ do
      srv <- getServer
      db  <- connect (wsUrl srv) defaultConnectOpts
      runSurreal db $ do
        _ <- signin (Root "root" "root")
        use (Just "ns3") (Just "db3")
        -- Create one record first so the table exists before LIVE SELECT.
        _  <- create (onTable "watched") (IPerson "Seed" 1) :: SurrealT IO [IPerson]
        lq <- live "watched"
        _  <- create (onTable "watched") (IPerson "Eve" 22) :: SurrealT IO [IPerson]
        mn <- liftIO (timeout 5000000 (nextNotification lq))
        liftIO (fmap ntfAction mn @?= Just ActionCreate)
        kill lq
      close db
  ]
