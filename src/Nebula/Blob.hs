{-# Language OverloadedStrings #-}

-- | Blobs are some chunk of data stored in Nebula; they can be considered
-- atoms. This module contains code for persisting and retrieving
-- blobs.
--
-- Blobs are identified by their hex-encoded SHA-256 digest. For example,
-- the string @\"Hello, world\"@ would be identified as
-- @\"4ae7c3b6ac0beff671efa8cf57386151c06e58ca53a78d83f36107316cec125f\"@.
-- They are stored on disk as the joining of some parent directory
-- containing the blobs and the split of the digest into each of its bytes.
--
--
-- >>> import qualified Nebula.Blob as Blob
-- >>> import qualified Nebula.Util as Util
-- >>> let blob = Util.toBytes "Hello, world"
-- >>> Blob.writeBlob "nebula-store" blob
-- "4ae7c3b6ac0beff671efa8cf57386151c06e58ca53a78d83f36107316cec125f"
-- >>> Blob.readBlob "nebula-store" "4ae7c3b6ac0beff671efa8cf57386151c06e58ca53a78d83f36107316cec125f"
-- Just "Hello, world"
-- >>> Blob.hashPath "nebula-store" "4ae7c3b6ac0beff671efa8cf57386151c06e58ca53a78d83f36107316cec125f"
-- "nebula-store/4a/e7/c3/b6/ac/0b/ef/f6/71/ef/a8/cf/57/38/61/51/c0/6e/58/ca/53/a7/8d/83/f3/61/07/31/6c/ec/12/5f"
--
-- Blobs are never accessed directly by users. Instead, they are
-- accessed via the "Nebula.Entry" module.
module Nebula.Blob
    (
      -- * General utility functions.
      hashPath
    , hashBlob

      -- * Blob persistence
    , writeBlob
    , readBlob
    ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Base16 (encode, decode)
import qualified System.FilePath as File
import qualified System.Directory as Dir

import qualified Nebula.Util as Util

-- |hashPath takes a parent directory and the hash identifier; it
-- produces a path suitable for storing a blob.
hashPath :: FilePath -> FilePath -> FilePath
hashPath p blob = p ++  buildHashPath blob
  where buildHashPath [] = []
        buildHashPath blob = "/" ++ (take 2 blob) ++ (buildHashPath $ drop 2 blob)

-- |testHashPath verifies that hashPath produces an appropriate path.
testHashPath = (hashPath p blob) == expected
  where p = "p"
        blob = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        expected = "p/00/01/02/03/04/05/06/07/08/09/0a/0b/0c/0d/0e/0f/10/11/12/13/14/15/16/17/18/19/1a/1b/1c/1d/1e/1f"

{- All SHA-256 hashes are represented as their hex-encoded equivalents. -}

-- |hashBlob produces a hex-encoded SHA-256 digest of its ByteString
-- input.
hashBlob :: BS.ByteString -> BS.ByteString
hashBlob = Data.ByteString.Base16.encode . SHA256.hash

{- Writing a blob to disk requires that the parent directory be created
   if it doesn't exist. -}

-- |Given a parent directory and a bytestring, writeBlob will write
-- the blob to disk and return the hex-encoded SHA-256 identifier.
writeBlob :: FilePath -> BS.ByteString -> IO String
writeBlob p blob = do
    Dir.createDirectoryIfMissing True $ File.takeDirectory path
    BS.writeFile path blob
    return $ BS.unpack hashed
  where hashed = hashBlob blob
        path   = hashPath p $ BS.unpack hashed

{- Reading a blob is now a case of checking whether the blob exists, and
   looking up the associated hash if so. -}

-- |readBlob takes a parent directory containing the blobs and a
-- string containing a hex-encoded hex digest; if the blob is
-- present, its contents will be returned.
readBlob :: FilePath -> String -> IO (Maybe BS.ByteString)
readBlob p hashed = do
  if valid then do
    blobExists <- Dir.doesFileExist path
    if blobExists
      then do
        contents <- BS.readFile path
        return $ Just contents
      else return $ Nothing
  else return $ Nothing
  where valid = Util.isValidHash hashed
        path = hashPath p hashed


