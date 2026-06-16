{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Concrete Haskell representations of SurrealDB's custom data types together
-- with the canonical 'SurrealValue' AST that every value travels through.
--
-- The data declarations live here. The wire instances (CBOR via @serialise@
-- and JSON via @aeson@) live in "Database.Surreal.Value", which is the only
-- module that encodes the tag and string conventions.
--
-- 'SurrealValue' and several of the custom types are mutually recursive (a
-- record id can itself contain an array or object of values, a range carries
-- bounds that are values), so they are defined together in a single module.
module Database.Surreal.Types
  ( -- * The value AST
    SurrealValue (..)

    -- * Tables and record ids
  , Table (..)
  , RecordId (..)
  , recordId
  , recordIdText
  , Target (..)

    -- * Ranges
  , Bound (..)
  , Range (..)
  , RecordIdRange (..)

    -- * Scalars
  , Uuid (..)
  , uuidFromText
  , uuidToText
  , Decimal (..)
  , DateTime (..)
  , dateTimeFromUTCTime
  , dateTimeToUTCTime
  , Duration (..)
  , durationFromText
  , durationToText
  , durationFromNanos
  , FileRef (..)
  , Future (..)

    -- * Geometry
  , Geometry (..)
  , Point
  ) where

import           Data.ByteString       (ByteString)
import           Data.Int              (Int64)
import           Data.Map.Strict       (Map)
import           Data.Scientific       (Scientific)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Read        as TR
import           Data.Time             (UTCTime)
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.UUID.Types       as UUID
import           Data.Vector           (Vector)
import           Data.Word             (Word32)
import           GHC.Generics          (Generic)

-- | The canonical value type. Every value sent to or received from SurrealDB is
-- represented as a 'SurrealValue' before it is handed to a codec, so the wire
-- conventions live in exactly one place.
data SurrealValue
  = VNull
  | VNone
  | VBool      !Bool
  | VInt       !Int64
  | VFloat     !Double
  | VDecimal   !Decimal
  | VString    !Text
  | VBytes     !ByteString
  | VArray     !(Vector SurrealValue)
  | VSet       !(Vector SurrealValue)
  | VObject    !(Map Text SurrealValue)
  | VUuid      !Uuid
  | VDateTime  !DateTime
  | VDuration  !Duration
  | VTable     !Table
  | VRecordId  !RecordId
  | VRange     !Range
  | VFuture    !Future
  | VFileRef   !FileRef
  | VGeometry  !Geometry
  deriving (Eq, Show, Generic)

-- | A table reference, for example @person@.
newtype Table = Table { unTable :: Text }
  deriving (Eq, Ord, Show, Generic)

-- | A record id such as @person:alice@. The identifier part may itself be any
-- value (string, integer, uuid, array or object), matching SurrealDB.
data RecordId = RecordId
  { ridTable :: !Text
  , ridId    :: !SurrealValue
  } deriving (Eq, Show, Generic)

-- | Build a record id from a table name and an identifier value.
recordId :: Text -> SurrealValue -> RecordId
recordId = RecordId

-- | Build a record id with a textual identifier, the most common case.
recordIdText :: Text -> Text -> RecordId
recordIdText tb i = RecordId tb (VString i)

-- | What a CRUD method addresses: a whole table, a single record, or a range
-- of records within a table.
data Target
  = TargetTable  !Table
  | TargetRecord !RecordId
  | TargetRange  !RecordIdRange
  deriving (Eq, Show, Generic)

-- | A range bound. SurrealDB distinguishes inclusive and exclusive bounds and
-- supports open ends.
data Bound
  = BoundIncluded !SurrealValue
  | BoundExcluded !SurrealValue
  | BoundUnbounded
  deriving (Eq, Show, Generic)

-- | A value range, for example @1..10@ or @1..=10@.
data Range = Range
  { rangeBegin :: !Bound
  , rangeEnd   :: !Bound
  } deriving (Eq, Show, Generic)

-- | A range of record ids within a single table, for example
-- @person:alice..person:bob@.
data RecordIdRange = RecordIdRange
  { rirTable :: !Text
  , rirBegin :: !Bound
  , rirEnd   :: !Bound
  } deriving (Eq, Show, Generic)

-- | A UUID value.
newtype Uuid = Uuid { unUuid :: UUID.UUID }
  deriving (Eq, Ord, Show, Generic)

-- | Parse a UUID from its canonical text form.
uuidFromText :: Text -> Maybe Uuid
uuidFromText = fmap Uuid . UUID.fromText

-- | Render a UUID to its canonical text form.
uuidToText :: Uuid -> Text
uuidToText = UUID.toText . unUuid

-- | An arbitrary precision decimal. Modelled over 'Scientific'.
newtype Decimal = Decimal { unDecimal :: Scientific }
  deriving (Eq, Ord, Show, Generic)

-- | A timestamp with nanosecond precision, stored as whole seconds plus a
-- nanosecond remainder. This mirrors SurrealDB's compact datetime and avoids
-- the sub-microsecond loss that 'UTCTime' would introduce.
data DateTime = DateTime
  { dtSeconds :: !Int64
  , dtNanos   :: !Word32
  } deriving (Eq, Ord, Show, Generic)

-- | Convert a 'UTCTime' to a SurrealDB datetime. Sub-nanosecond detail in the
-- source value is truncated.
dateTimeFromUTCTime :: UTCTime -> DateTime
dateTimeFromUTCTime t =
  let p           = POSIX.utcTimeToPOSIXSeconds t
      whole       = floor p :: Int64
      frac        = p - fromIntegral whole
      nanos       = round (frac * 1e9) :: Word32
  in DateTime whole nanos

-- | Convert a SurrealDB datetime back to a 'UTCTime'.
dateTimeToUTCTime :: DateTime -> UTCTime
dateTimeToUTCTime (DateTime s n) =
  POSIX.posixSecondsToUTCTime
    (fromIntegral s + fromIntegral n / 1e9)

-- | A duration with nanosecond precision.
data Duration = Duration
  { durSeconds :: !Int64
  , durNanos   :: !Word32
  } deriving (Eq, Ord, Show, Generic)

-- | Build a duration from a total number of nanoseconds.
durationFromNanos :: Integer -> Duration
durationFromNanos total =
  let (s, n) = total `divMod` 1000000000
  in Duration (fromIntegral s) (fromIntegral n)

-- | Render a duration to SurrealDB's compact text form, for example
-- @1h30m@ or @500ms@. A zero duration renders as @0ns@.
durationToText :: Duration -> Text
durationToText (Duration s n)
  | s == 0 && n == 0 = "0ns"
  | otherwise        =
      T.concat (weeks ++ days ++ hours ++ mins ++ secs ++ millis ++ micros ++ nanos)
  where
    unit q label = [T.pack (show q) <> label | q > 0]
    (w, rW)   = s `divMod` 604800
    (d, rD)   = rW `divMod` 86400
    (h, rH)   = rD `divMod` 3600
    (m, rM)   = rH `divMod` 60
    sec       = rM
    (ms, rMs) = n `divMod` 1000000
    (us, rUs) = rMs `divMod` 1000
    ns        = rUs
    weeks  = unit w "w"
    days   = unit d "d"
    hours  = unit h "h"
    mins   = unit m "m"
    secs   = unit sec "s"
    millis = unit (fromIntegral ms :: Int64) "ms"
    micros = unit (fromIntegral us :: Int64) "us"
    nanos  = unit (fromIntegral ns :: Int64) "ns"

-- | Parse a duration from SurrealDB's compact text form. Recognises the units
-- @w@, @d@, @h@, @m@, @s@, @ms@, @us@ (and the micro sign spelling) and @ns@.
durationFromText :: Text -> Maybe Duration
durationFromText = go 0 . T.strip
  where
    go :: Integer -> Text -> Maybe Duration
    go !acc t
      | T.null t  = Just (durationFromNanos acc)
      | otherwise = do
          let (digits, rest) = T.span (\c -> c >= '0' && c <= '9') t
          q <- if T.null digits
                 then Nothing
                 else either (const Nothing) (Just . fst) (TR.decimal digits :: Either String (Integer, Text))
          (mult, rest') <- unit rest
          go (acc + q * mult) rest'

    unit t
      | Just r <- T.stripPrefix "ns"   t = Just (1, r)
      | Just r <- T.stripPrefix "us"   t = Just (1000, r)
      | Just r <- T.stripPrefix "\181s" t = Just (1000, r)
      | Just r <- T.stripPrefix "ms"   t = Just (1000000, r)
      | Just r <- T.stripPrefix "w"    t = Just (604800 * nano, r)
      | Just r <- T.stripPrefix "d"    t = Just (86400 * nano, r)
      | Just r <- T.stripPrefix "h"    t = Just (3600 * nano, r)
      | Just r <- T.stripPrefix "m"    t = Just (60 * nano, r)
      | Just r <- T.stripPrefix "s"    t = Just (nano, r)
      | otherwise                        = Nothing
    nano = 1000000000

-- | A reference to a file stored in a bucket, for example @bucket:/key@.
data FileRef = FileRef
  { frBucket :: !Text
  , frKey    :: !Text
  } deriving (Eq, Show, Generic)

-- | An uncomputed future expression. SurrealDB returns these tagged; clients
-- usually receive computed values instead.
newtype Future = Future { unFuture :: SurrealValue }
  deriving (Eq, Show, Generic)

-- | A two dimensional coordinate, longitude first to match GeoJSON.
type Point = (Double, Double)

-- | GeoJSON compatible geometry values.
data Geometry
  = GeometryPoint        !Point
  | GeometryLine         !(Vector Point)
  | GeometryPolygon      !(Vector (Vector Point))
  | GeometryMultiPoint   !(Vector Point)
  | GeometryMultiLine    !(Vector (Vector Point))
  | GeometryMultiPolygon !(Vector (Vector (Vector Point)))
  | GeometryCollection   !(Vector Geometry)
  deriving (Eq, Show, Generic)
