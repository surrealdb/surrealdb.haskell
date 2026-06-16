{-# LANGUAGE DeriveGeneric #-}

-- | The error hierarchy. 'SurrealError' is the single exception type thrown by
-- the public API. 'DecodeError' is returned purely by
-- 'Database.Surreal.Class.SurrealRecord' and only lifted into 'SurrealError' at
-- the API boundary, so the value mapping layer stays exception free and easy to
-- test.
module Database.Surreal.Error
  ( SurrealError (..)
  , ConnectError (..)
  , TransportError (..)
  , ProtocolError (..)
  , CodecError (..)
  , DecodeError (..)
  , RpcError (..)
  , AuthError (..)
  ) where

import           Control.Exception (Exception)
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

-- | The sole exception thrown by "Database.Surreal.Api" and the connection
-- layer. The variants separate the failure modes a caller needs to distinguish
-- when writing a retry or recovery policy.
data SurrealError
  = ConnectErr      ConnectError
  | TransportErr    TransportError
  | ProtocolErr     ProtocolError
  | CodecErr        CodecError
  | DecodeErr       DecodeError
  | RpcErr          RpcError
  | AuthErr         AuthError
  | UnsupportedByTransport Text
    -- ^ The method is not available on the active transport, for example a
    -- live query over HTTP. Carries the method name.
  | TimeoutErr      Text Int
    -- ^ An RPC was awaited longer than the configured timeout. Carries the
    -- method name and the timeout in microseconds.
  | ConnectionLost
    -- ^ The connection dropped while calls were in flight.
  deriving (Eq, Show, Generic)

instance Exception SurrealError

-- | Failures while establishing a connection.
data ConnectError
  = BadEndpoint            Text
  | HandshakeFailed        Text
  | CodecNegotiationFailed Text
  deriving (Eq, Show, Generic)

-- | Failures on an established transport.
data TransportError
  = SocketClosed
  | WriteFailed Text
  | HttpStatus  Int Text
  deriving (Eq, Show, Generic)

-- | The peer sent something that did not fit the protocol.
data ProtocolError
  = UnexpectedFrame     Text
  | UnknownResponseId   Integer
  | MissingResultAndError
  deriving (Eq, Show, Generic)

-- | The wire bytes could not be decoded by the active codec.
data CodecError
  = CborDecodeFailed Text
  | JsonDecodeFailed Text
  deriving (Eq, Show, Generic)

-- | The bytes decoded fine but did not fit the requested Haskell type. Returned
-- purely by 'Database.Surreal.Class.SurrealRecord'.
data DecodeError
  = TypeMismatch Text Text
    -- ^ Expected and actual descriptions.
  | MissingField Text
  | UnexpectedField Text
  | BadEnumValue Text
  | NestedDecode Text DecodeError
  | DecodeMessage Text
  deriving (Eq, Show, Generic)

-- | An error result returned by the server, mirroring its @{ code, message }@
-- shape.
data RpcError = RpcError
  { rpcCode    :: !Int
  , rpcMessage :: !Text
  } deriving (Eq, Show, Generic)

-- | Authentication failures.
data AuthError
  = InvalidCredentials
  | TokenExpired
  | NotAuthenticated
  deriving (Eq, Show, Generic)
