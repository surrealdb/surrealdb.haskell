{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the 'SurrealRecord' mapping and the duration text format.
module Test.Surreal.ValueSpec (tests) where

import           Data.Text       (Text)
import           GHC.Generics    (Generic)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Database.Surreal.Class
import           Database.Surreal.Error (DecodeError (..))
import           Database.Surreal.Types
import           Test.Surreal.Gen (genDuration)

data Person = Person
  { name   :: Text
  , age    :: Int
  , active :: Bool
  } deriving (Eq, Show, Generic)

instance SurrealRecord Person

data Nested = Nested
  { title :: Text
  , owner :: Person
  } deriving (Eq, Show, Generic)

instance SurrealRecord Nested

newtype Optional = Optional
  { nickname :: Maybe Text
  } deriving (Eq, Show, Generic)

instance SurrealRecord Optional

tests :: TestTree
tests = testGroup "Value"
  [ testCase "record round trips through SurrealValue" $
      let p = Person "Alice" 30 True
      in fromSurreal (toSurreal p) @?= Right p

  , testCase "nested record round trips" $
      let n = Nested "boss" (Person "Bob" 40 False)
      in fromSurreal (toSurreal n) @?= Right n

  , testCase "missing optional record field decodes to Nothing" $
      fromSurreal (VObject mempty) @?= Right (Optional Nothing)

  , testCase "type mismatch is reported" $
      case (fromSurreal (VString "x") :: Either DecodeError Int) of
        Left TypeMismatch{} -> pure ()
        other               -> assertFailure ("expected TypeMismatch, got " ++ show other)

  , testProperty "duration text round trips" $
      forAll genDuration $ \d ->
        durationFromText (durationToText d) === Just d
  ]
