> {-# LANGUAGE OverloadedStrings #-}
> module Nebula.Blob (
>   hashPath
> , hashBlob
> , writeBlob
> , readBlob
> ) where
>
> import Crypto.Hash.SHA256 as SHA256
> import qualified Data.ByteString.Char8 as BS
> import qualified Data.ByteString.Lazy.Internal as LBS
> import qualified Data.ByteString.Base16 (encode, decode)
> import qualified System.FilePath as File
> import qualified System.Directory as Dir
>
> import qualified Nebula.Util as Util

Data is referred to by the SHA-256 hash of the contents of the
file. Nebula stores blobs in a directory consisting of a parent
directory joined with each byte as a separate path component.

Example: given the SHA-256 ID and a parent `p`:

000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f

The result is

p/00/01/02/03/04/05/06/07/08/09/0a/0b/0c/0d/0e/0f/10/11/12/13/14/15/16/17/18/19/1a/1b/1c/1d/1e/1f

> hashPath :: FilePath -> FilePath -> FilePath
> hashPath p blob = p ++  buildHashPath blob
>   where buildHashPath [] = []
>         buildHashPath blob = "/" ++ (take 2 blob) ++ (buildHashPath $ drop 2 blob)

This function can be checked against the example listed previously:

> testHashPath = (hashPath p blob) == expected
>   where p = "p"
>         blob = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
>         expected = "p/00/01/02/03/04/05/06/07/08/09/0a/0b/0c/0d/0e/0f/10/11/12/13/14/15/16/17/18/19/1a/1b/1c/1d/1e/1f"

All SHA-256 hashes are represented as their hex-encoded equivalents.

> hashBlob :: BS.ByteString -> BS.ByteString
> hashBlob = Data.ByteString.Base16.encode . SHA256.hash

Writing a blob to disk requires that the parent directory be created
if it doesn't exist.

> writeBlob :: FilePath -> BS.ByteString -> IO String
> writeBlob p blob = do
>     Dir.createDirectoryIfMissing True $ File.takeDirectory path
>     BS.writeFile path blob
>     return $ BS.unpack hashed
>   where hashed = hashBlob blob
>         path   = hashPath p $ BS.unpack hashed

Reading a blob is now a case of checking whether the blob exists, and
looking up the associated hash if so.

> readBlob :: FilePath -> String -> IO (Maybe BS.ByteString)
> readBlob p hashed = do
>   if valid then do
>     blobExists <- Dir.doesFileExist path
>     if blobExists
>       then do
>         contents <- BS.readFile path
>         return $ Just contents
>       else return $ Nothing
>   else return $ Nothing
>   where valid = Util.isValidHash hashed
>         path = hashPath p hashed

