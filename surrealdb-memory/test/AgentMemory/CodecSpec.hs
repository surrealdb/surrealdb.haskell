{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for the Agent Memory request encoding and error classification. No
-- server is required.
module AgentMemory.CodecSpec (tests) where

import           Data.Aeson       (encode, toJSON)
import           Test.Tasty
import           Test.Tasty.HUnit

import           AgentMemory.Error
import           AgentMemory.Types

tests :: TestTree
tests = testGroup "AgentMemory"
  [ testCase "scope normalises empties and duplicates" $
      normaliseScope (Scope [["team/eng", ""], [], ["team/eng"]])
        @?= Scope [["team/eng"]]

  , testCase "scope renders as an array of arrays" $
      scopeToJSON (Scope [["a"], ["b", "c"]])
        @?= toJSON ([["a"], ["b", "c"]] :: [[String]])

  , testCase "batch message encodes role and content" $
      encode (BatchMessage RoleUser "hello")
        @?= "{\"content\":\"hello\",\"role\":\"user\"}"

  , testCase "objectMaybe omits absent fields" $
      encode (objectMaybe [ "a" .=? Just (1 :: Int), "b" .=? (Nothing :: Maybe Int) ])
        @?= "{\"a\":1}"

  , testCase "status codes classify correctly" $ do
      classifyStatus 401 @?= AuthFailed
      classifyStatus 403 @?= ScopeRejected
      classifyStatus 404 @?= NotFound
      classifyStatus 422 @?= ValidationFailed
      classifyStatus 429 @?= RateLimited
      classifyStatus 503 @?= ServerFailed
  ]
