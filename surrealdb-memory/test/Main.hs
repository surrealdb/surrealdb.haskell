-- | The surrealdb-memory test entry point. These tests are pure and need no
-- running Agent Memory service.
module Main (main) where

import           Test.Tasty

import qualified AgentMemory.CodecSpec as Codec

main :: IO ()
main = defaultMain (testGroup "surrealdb-memory" [Codec.tests])
