{-# LANGUAGE DeriveAnyClass #-}

module Noided.Web.Error where

import Data.Text (Text)
import GHC.Generics (Generic)
import Noided.Validation (ValidationError)

-- | Bad request error: the request was not valid (HTTP 400).
newtype BadRequest = BadRequest {message :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

-- | Unauthorized error: authentication is required (HTTP 401).
newtype Unauthorized = Unauthorized {message :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

-- | Forbidden error: the client does not have access rights (HTTP 403).
newtype Forbidden = Forbidden {message :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

-- | Not found error: the requested resource does not exist (HTTP 404).
newtype NotFound = NotFound {message :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

-- | Conflict error: the request conflicts with the current state (HTTP 409).
newtype Conflict = Conflict {message :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

-- | I am a teapot error (HTTP 418).
newtype IAmATeapot = IAmATeapot {message :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

-- | Too many requests error: the client has sent too many requests (HTTP 429).
newtype TooManyRequests = TooManyRequests {message :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

-- | Unavailable for legal reasons error (HTTP 451).
newtype UnavailableForLegalReasons = UnavailableForLegalReasons {message :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ValidationError)
