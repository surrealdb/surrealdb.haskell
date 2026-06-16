-- | The surrealdb-spectron test entry point. These tests are pure and need no
-- running Spectron service.
module Main (main) where

import           Test.Tasty

import qualified Spectron.CodecSpec as Codec

main :: IO ()
main = defaultMain (testGroup "surrealdb-spectron" [Codec.tests])
