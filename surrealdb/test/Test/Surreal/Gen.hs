{-# LANGUAGE OverloadedStrings #-}

-- | QuickCheck generators for 'SurrealValue' and the custom types. Two value
-- generators are provided: 'genValue' covers every variant for the CBOR round
-- trip, while 'genJsonValue' is restricted to the JSON representable subset.
module Test.Surreal.Gen
  ( genValue
  , genJsonValue
  , genDuration
  , genDateTime
  ) where

import qualified Data.ByteString as BS
import           Data.Int        (Int64)
import qualified Data.Map.Strict as Map
import           Data.Scientific (scientific)
import qualified Data.Text       as T
import qualified Data.UUID.Types as UUID
import qualified Data.Vector     as V
import           Data.Word       (Word32, Word8)
import           Test.QuickCheck

import           Database.Surreal.Types

genText :: Gen T.Text
genText = T.pack <$> listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

genDouble :: Gen Double
genDouble = do
  n <- arbitrary :: Gen Int
  f <- choose (0, 0.999) :: Gen Double
  pure (fromIntegral n + f)

genDecimal :: Gen Decimal
genDecimal = do
  c <- arbitrary :: Gen Integer
  e <- choose (-6, 6)
  pure (Decimal (scientific c e))

genUuid :: Gen Uuid
genUuid = Uuid <$> (UUID.fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

genDateTime :: Gen DateTime
genDateTime = DateTime <$> (arbitrary :: Gen Int64) <*> (fromIntegral <$> choose (0, 999999999 :: Int))

genDuration :: Gen Duration
genDuration = do
  s <- abs <$> (arbitrary :: Gen Int64)
  n <- fromIntegral <$> choose (0, 999999999 :: Int) :: Gen Word32
  pure (Duration s n)

genBytes :: Gen BS.ByteString
genBytes = BS.pack <$> listOf (arbitrary :: Gen Word8)

genPoint :: Gen Point
genPoint = (,) <$> genDouble <*> genDouble

genBound :: Int -> Gen Bound
genBound d = oneof
  [ BoundIncluded <$> genValue (d - 1)
  , BoundExcluded <$> genValue (d - 1)
  , pure BoundUnbounded
  ]

genGeometry :: Gen Geometry
genGeometry = oneof
  [ GeometryPoint <$> genPoint
  , GeometryLine <$> (V.fromList <$> listOf1 genPoint)
  , GeometryPolygon <$> (V.fromList <$> listOf1 (V.fromList <$> listOf1 genPoint))
  , GeometryMultiPoint <$> (V.fromList <$> listOf1 genPoint)
  , GeometryMultiLine <$> (V.fromList <$> listOf1 (V.fromList <$> listOf1 genPoint))
  , GeometryMultiPolygon <$> (V.fromList <$> listOf1 (V.fromList <$> listOf1 (V.fromList <$> listOf1 genPoint)))
  ]

-- | A generator covering every value variant, with bounded recursion depth.
genValue :: Int -> Gen SurrealValue
genValue d
  | d <= 0 = leaf
  | otherwise = frequency [(6, leaf), (1, nested)]
  where
    leaf = oneof
      [ pure VNull
      , pure VNone
      , VBool <$> arbitrary
      , VInt <$> arbitrary
      , VFloat <$> genDouble
      , VDecimal <$> genDecimal
      , VString <$> genText
      , VBytes <$> genBytes
      , VUuid <$> genUuid
      , VDateTime <$> genDateTime
      , VDuration <$> genDuration
      , VTable . Table <$> genText
      , VGeometry <$> genGeometry
      , VFileRef <$> (FileRef <$> genText <*> genText)
      ]
    nested = oneof
      [ VArray . V.fromList <$> resize 3 (listOf (genValue (d - 1)))
      , VSet . V.fromList <$> resize 3 (listOf (genValue (d - 1)))
      , VObject . Map.fromList <$> resize 3 (listOf ((,) <$> genText <*> genValue (d - 1)))
      , VRecordId <$> (RecordId <$> genText <*> genValue (d - 1))
      , VRange <$> (Range <$> genBound d <*> genBound d)
      , VFuture . Future <$> genValue (d - 1)
      ]

-- | A generator restricted to the values that survive a JSON round trip.
genJsonValue :: Int -> Gen SurrealValue
genJsonValue d
  | d <= 0 = leaf
  | otherwise = frequency [(6, leaf), (1, nested)]
  where
    leaf = oneof
      [ pure VNull
      , VBool <$> arbitrary
      , VInt <$> arbitrary
      , VFloat . (\n -> fromIntegral n + 0.5) <$> (arbitrary :: Gen Int)
      , VString <$> genText
      ]
    nested = oneof
      [ VArray . V.fromList <$> resize 3 (listOf (genJsonValue (d - 1)))
      , VObject . Map.fromList <$> resize 3 (listOf ((,) <$> genText <*> genJsonValue (d - 1)))
      ]
