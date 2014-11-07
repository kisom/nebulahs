> import Control.Monad
> import Crypto.Hash.SHA256 as SHA256
> import qualified Data.ByteString as BS
> import qualified Data.ByteString.Base16 (encode, decode)
> import System.IO.Unsafe

Introduction
------------

Nebula is a toy file storage system written in Literate Haskell.

I need a project to play around with the practicals of Haskell. To me,
practical uses are why I choose a given programming language mostly,
and in this case, what I want to do intersects my systems and networking
background. This idea is inspired by a talk from Joe Armstrong, from some
previous ideas ve hacked on, and from talking on the tyrfingr IRC channel.

The basic idea is a file storage system, available via a network
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
>    Prelude.putStrLn "Les nuages noirs Font la mer noire."

The `fileHash` function has been challenging to write. The core problem is
this: how do I read the contents of a file, compute the SHA256 digest of
this, and then return a hex-encoded string containing the SHA256 digest?

Ideally, this would have a type of

```haskell
fileHash :: FilePath -> Maybe String
```

A `Nothing` value would indicate that there was an error reading the
file; maybe the file doesn't exist.

Unfortunately, based on what I know thus far in Haskell, I have to resort
to `unsafePerformIO`. Is this a huge warning bell I hear going off in
my head? Why, yes it is, gentle reader.

> fileHash path = Data.ByteString.Base16.encode $ SHA256.hash contents
>     where contents = unsafePerformIO $ BS.readFile path

As mentioned in the introduction, a file path is placed in a directory
where the first two bytes (or four characters in a hex-encoded string)
are the parent directory. `hashPath` takes a hash, and generates this
same path structure.

> -- hashPath :: BS.ByteString -> FilePath
> hashPath h = BS.append (BS.take 4 h) $ BS.append sep (BS.drop 4 h)

This fills me with shame. There **has** to be a better way.

>     where sep = BS.replicate 1 47

Getting the parent directory of a file means extracting the first two
bytes from the digest.

> hashPathParent h = BS.take 4 h

The `FileBase` type indicates whether the digest is referring to the
content or metadata of an identifier.

> data FileBase a = Content a | MetaData a deriving (Show)
