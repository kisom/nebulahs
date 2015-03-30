> module Nebula.Util (
>   isValidHash
> , isValidUUID
> ) where

> import qualified Data.UUID as UUID

This module contains various utility functions to be utilised across
Nebula.

It's useful to determine whether something is a valid SHA-256
identifier.

First, the valid characters and the expected length of a SHA-256 hash
(keeping in mind that in Nebula, a SHA-256 hash is always referred to
as in its hex-encoded form).

> hexAlphabet = "0123456789abcdef"
> sha256Length = 64

> isValidHash :: String -> Bool
> isValidHash x = (length filtered) == sha256Length
>   where filtered = filter (flip elem hexAlphabet) x

It's also useful to validate a UUID. For this, the UUID parser from
the UUID package is used.

> isValidUUID :: String -> Bool
> isValidUUID uuid = case parsed of
>   Nothing -> False
>   Just _  -> True
>   where parsed = UUID.fromString uuid
