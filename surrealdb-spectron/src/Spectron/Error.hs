{-# LANGUAGE OverloadedStrings #-}

-- | The Spectron error type. HTTP problem responses are mapped to a kind so
-- callers can branch on the failure mode.
module Spectron.Error
  ( SpectronError (..)
  , ErrorKind (..)
  , classifyStatus
  ) where

import           Control.Exception (Exception)
import           Data.Text         (Text)

-- | An error returned by the Spectron API or raised while talking to it.
data SpectronError = SpectronError
  { seStatus :: !Int
  , seKind   :: !ErrorKind
  , seTitle  :: !Text
  , seDetail :: !(Maybe Text)
  } deriving (Eq, Show)

instance Exception SpectronError

-- | The classified failure mode.
data ErrorKind
  = AuthFailed        -- ^ 401, missing or invalid token.
  | ScopeRejected     -- ^ 403, principal or scope floor rejected the call.
  | NotFound          -- ^ 404.
  | ValidationFailed  -- ^ 400 or 422.
  | RateLimited       -- ^ 429.
  | ServerFailed      -- ^ 5xx after retries.
  | ConnectionFailed  -- ^ Network failure, timeout or non HTTP error.
  deriving (Eq, Show)

-- | Map an HTTP status code to an error kind.
classifyStatus :: Int -> ErrorKind
classifyStatus s
  | s == 401            = AuthFailed
  | s == 403            = ScopeRejected
  | s == 404            = NotFound
  | s == 400 || s == 422 = ValidationFailed
  | s == 429            = RateLimited
  | s >= 500            = ServerFailed
  | otherwise           = ConnectionFailed
