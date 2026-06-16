-- | The wire instances for 'SurrealValue'. This module owns the
-- 'Codec.Serialise.Serialise' (CBOR) and 'Data.Aeson.ToJSON' \/
-- 'Data.Aeson.FromJSON' (JSON) instances, delegating to the per-codec helpers
-- in "Database.Surreal.Value.Cbor" and "Database.Surreal.Value.Json".
--
-- Because these are the only instances in the codebase, all of SurrealDB's tag
-- and string conventions live behind this one module. User types never gain
-- their own wire instances; they convert to and from 'SurrealValue' through the
-- 'Database.Surreal.Class.SurrealRecord' class and stay codec blind.
module Database.Surreal.Value
  ( module Database.Surreal.Types
  ) where

import           Codec.Serialise            (Serialise (..))
import           Data.Aeson                 (FromJSON (..), ToJSON (..))

import           Database.Surreal.Types
import qualified Database.Surreal.Value.Cbor as Cbor
import qualified Database.Surreal.Value.Json as Json

instance Serialise SurrealValue where
  encode = Cbor.encodeSurreal
  decode = Cbor.decodeSurreal

instance ToJSON SurrealValue where
  toJSON = Json.toJSONValue

instance FromJSON SurrealValue where
  parseJSON = pure . Json.fromJSONValue
