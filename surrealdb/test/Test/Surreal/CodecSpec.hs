{-# LANGUAGE OverloadedStrings #-}

-- | Codec round trip tests. CBOR round trips every value variant; JSON round
-- trips its representable subset; and the custom type JSON encodings are
-- checked against SurrealDB's string conventions.
module Test.Surreal.CodecSpec (tests) where

import qualified Codec.Serialise      as CBOR
import qualified Data.Aeson           as A
import qualified Data.Map.Strict      as Map
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Database.Surreal.Types
import           Database.Surreal.Value ()
import           Test.Surreal.Gen

tests :: TestTree
tests = testGroup "Codec"
  [ testProperty "CBOR round trips every value" $
      forAll (genValue 4) $ \v ->
        CBOR.deserialiseOrFail (CBOR.serialise v) === Right v

  , testProperty "JSON round trips the representable subset" $
      forAll (genJsonValue 4) $ \v ->
        A.eitherDecode (A.encode v) === Right v

  , testCase "record id encodes as table:id in JSON" $
      A.encode (VRecordId (recordIdText "person" "alice")) @?= "\"person:alice\""

  , testCase "table encodes as its name in JSON" $
      A.encode (VTable (Table "person")) @?= "\"person\""

  , testCase "none encodes as null in JSON" $
      A.encode VNone @?= "null"

  , testCase "duration encodes as compact text in JSON" $
      A.encode (VDuration (Duration 5400 0)) @?= "\"1h30m\""

  , testCase "object keys are preserved over CBOR" $
      let v = VObject (Map.fromList [("a", VInt 1), ("b", VString "x")])
      in CBOR.deserialiseOrFail (CBOR.serialise v) @?= Right v
  ]
