{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The single dual codec seam. A 'Codec' is a runtime value carried by the
-- connection handle, never a type parameter, so every method above this layer
-- is codec agnostic by construction.
module Database.Surreal.Codec
  ( Codec (..)
  , codecSubprotocol
  , codecContentType
  , encodeValue
  , decodeValue
  ) where

import qualified Codec.Serialise        as CBOR
import qualified Data.Aeson             as A
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Database.Surreal.Error
import           Database.Surreal.Value ()
import           Database.Surreal.Types (SurrealValue)

-- | The wire codec negotiated for a connection. CBOR is the default and the
-- only fully typed option; JSON is provided for environments that need it.
data Codec
  = CodecCbor
  | CodecJson
  deriving (Eq, Show)

-- | The WebSocket subprotocol string for a codec.
codecSubprotocol :: Codec -> BS.ByteString
codecSubprotocol = \case
  CodecCbor -> "cbor"
  CodecJson -> "json"

-- | The HTTP @Content-Type@ and @Accept@ value for a codec.
codecContentType :: Codec -> BS.ByteString
codecContentType = \case
  CodecCbor -> "application/cbor"
  CodecJson -> "application/json"

-- | Encode a value with the given codec.
encodeValue :: Codec -> SurrealValue -> BL.ByteString
encodeValue CodecCbor = CBOR.serialise
encodeValue CodecJson = A.encode

-- | Decode a value with the given codec.
decodeValue :: Codec -> BL.ByteString -> Either SurrealError SurrealValue
decodeValue CodecCbor bs =
  case CBOR.deserialiseOrFail bs of
    Right v -> Right v
    Left e  -> Left (CodecErr (CborDecodeFailed (T.pack (show e))))
decodeValue CodecJson bs =
  case A.eitherDecode bs of
    Right v -> Right v
    Left e  -> Left (CodecErr (JsonDecodeFailed (T.pack e)))
