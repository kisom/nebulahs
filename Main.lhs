> {-# LANGUAGE OverloadedStrings #-}
> import Control.Applicative
> import Control.Monad
> import Crypto.Hash.SHA256 as SHA256
> import Data.Int
> import qualified Data.ByteString.Char8 as BS
> import qualified Data.ByteString.Lazy.Internal as LBS
> import qualified Data.ByteString.Base16 (encode, decode)
> import Snap.Core
> import Snap.Http.Server
> import System.Directory as Dir
> import System.IO.Unsafe

Introduction
------------

Nebula is a toy file storage system written in Literate Haskell.

I need a project to play around with the practicals of Haskell. To me,
practical uses are why I choose a given programming language mostly,
and in this case, what I want to do intersects my systems and networking
background. This idea is inspired by a talk from Joe Armstrong, from some
previous ideas ve hacked on, and from talking on the tyrfingr IRC channel.

nThe basic idea is a file storage system, available via a network
connection. Files are identified by a SHA256 hash, preferably with some
metadata associated (such as creation time). A file is immutable; once
entered, it doesn't change.

This source code is as much intended to be a notebook documenting
explorations into the Haskell language as it is a working program.


Basic idea
----------

The user can upload a stream of bytes (the file); the server will
perform a SHA256 hash of the file and return this to the user. The
file will be stored on disk in a file/<first two bytes of hash>/<rest
of hash> directory. 

For example, the file containing

```
Hello, world.
```

would be stored in

```
files/23eb/efdbe49a24e4bb9e243f649b3606c9241871a02d6122831d3cb7b2d6760~
```

Some metadata might be entered in

```
meta/23eb/7efdbe49a24e4bb9e243f649b3606c9241871a02d6122831d3cb7b2d6760
```


Status
------

Nebula is still a work in progress. The sequence of goals I am trying
to explore with this are:

1. Basic file storage and upload.
2. File history and keeping file history trees for versioning.
3. Basic metadata support.
4. Access control lists.
5. Encrypting files on disk.

> main = do
>    -- Prelude.putStrLn "Les nuages noirs Font la mer noire."
>   quickHttpServe site

> nebulaParent = "nebula/files"

- - -

File hashing

As mentioned in the introduction, a file path is placed in a directory
where the first two bytes (or four characters in a hex-encoded string)
are the parent directory. `hashPath` takes a hash, and generates this
same path structure.

> hashPath :: FilePath -> FilePath
> hashPath h = nebulaParent ++ "/" ++ (take 4 h) ++ "/" ++ (drop 4 h)

Getting the parent directory of a file means extracting the first two
bytes from the digest.

> hashPathParent :: FilePath -> FilePath
> hashPathParent h = nebulaParent ++ "/" ++ take 4 h

Given some content, we need to write it to disk and return the hash of the file.

> writeHash :: BS.ByteString -> IO BS.ByteString
> writeHash contents = (Dir.createDirectoryIfMissing True parent)
>                      >> BS.writeFile path contents
>                      >> return hash
>   where
>     hash   = Data.ByteString.Base16.encode $ SHA256.hash contents
>     path   = hashPath $ BS.unpack hash
>     parent = hashPathParent $ BS.unpack hash

Reading a hash first requires checking whether the hash is present,
and reading it if it is. I don't know how to handle switching out the
IO monad into the Snap monad, so I unsafePerformIO.

> readHash :: BS.ByteString -> Maybe BS.ByteString
> readHash h = case unsafePerformIO (Dir.doesFileExist hp) of
>     True  -> Just (unsafePerformIO $ BS.readFile hp)
>     False -> Nothing
>   where hp = hashPath $ BS.unpack h

- - -

Web "API"

> site :: Snap ()
> site = ifTop rootHandler <|> route [(":id", fileHandler)]

We'll need to read the entire file in to hash it, so the `slurp`
function will unpack and repack a file.

> slurp :: LBS.ByteString -> BS.ByteString
> slurp = BS.pack . LBS.unpackChars

The `rootHandler` should accept POST requests containing file data,
and store that file. maxFileSize is the maximum size of a file that
we'll accept (currently 32MB).

> maxFileSize = 33554432 :: Int64

> rootHandler :: Snap ()
> rootHandler = do
>   body <- readRequestBody maxFileSize
>   let hash = unsafePerformIO . writeHash $ slurp body
>   writeBS hash

The `fileHandler` should take a content hash and return the associated
file.

> fileHandler :: Snap ()
> fileHandler = do
>   fileID <- getParam "id"
>   case fileID of
>      Nothing -> writeBS "no such file"
>      Just h  -> case readHash h of
>        Nothing -> writeBS "read failed"
>        Just bs -> writeBS bs

