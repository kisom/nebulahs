{-|

    Utility functions that are used in multiple "Nebula" modules are
    placed in here. This module should be safe for importing into any
    other "Nebula" module.

 -}

module Nebula.Util
    (
      -- * Validation functions
      isValidHash
    , isValidUUID

      -- * Miscellaneous
    , getTime

      -- * REPL utilities
    , toBytes, fromBytes
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as UUID
import qualified Data.Int as Int
import qualified Data.Time.Exts.Unix as Time

{- This module contains various utility functions to be utilised across
   Nebula.

   It's useful to determine whether something is a valid SHA-256
   identifier.

   First, the valid characters and the expected length of a SHA-256 hash
   (keeping in mind that in Nebula, a SHA-256 hash is always referred to
   as in its hex-encoded form).
 -}

hexAlphabet = "0123456789abcdef"
sha256Length = 64

-- | Verify that the String represents a /possible/ valid SHA-256
-- identifier. This function therefore checks that the length is
-- appropriate and that only hex digits are present.
isValidHash :: String -> Bool
isValidHash x = (length $ filter (flip elem hexAlphabet) x) == sha256Length

{- It's also useful to validate a UUID. For this, the UUID parser from
   the UUID package is used. -}

-- | Verify that the String represents a valid UUID. Right now, the
-- string may be any of the versions of UUID.
isValidUUID :: String -> Bool
isValidUUID uuid = case UUID.fromString uuid of
  Nothing -> False
  Just _  -> True

{- Nebula also uses integer Unix timestamps. -}

-- | getTime returns the current Unix time in nanoseconds. Due to
-- platform issues, this may only be valid to microsecond resolution.
getTime :: IO Int.Int64
getTime = fmap Time.unixBase Time.getCurrentUnixDateTimeNanos

{- The following are useful for working with and converting between
   Strings and ByteStrings in the REPL. -}

-- |Convert a String to a ByteString.
toBytes = BS.pack

-- |Convert a ByteString into a String.
fromBytes = BS.unpack
