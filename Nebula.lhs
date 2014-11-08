> import Crypto.Hash.SHA256 as SHA256
> import qualified Data.ByteString.Char8 as BS
> import qualified Data.ByteString.Base16 (encode, decode)
> import System.Directory as Dir
> import System.IO.Unsafe as Unsafe

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


Of ByteStrings and Things
-------------------------

Nebula uses the `Char8` byte strings. Files are treated as binary
content, which means that they can be safely represented as bytes, or
`Char`s, as it were.

Core Functions
--------------

There are two core functions; these directly provide the functionality
desired of `Nebula`. Once these two functions are sketched out, the
remainder of the program falls into place.

The first of these functions is `readFile`. Given a SHA-256 digest
identifying the file, Nebula should retrieve the contents of the
file. The type definition expresses the intended behaviour: given a
`ByteString` that identifies a SHA-256 digest, return a `Maybe
ByteString` where a value of `Nothing` is indicative of a failure to
read the file.

> readFile :: BS.ByteString -> Maybe BS.ByteString
> readFile digest =

Given the digest, we'll need to convert it into a file path.

>     Just . Unsafe.unsafePerformIO $ BS.readFile $ pathFromDigest digest

Now that there is a definition for `readFile`, the `pathFromDigest`
function needs to defined. This takes a digest (which is a
`ByteString`) and returns a `FilePath`.

> pathFromDigest :: BS.ByteString -> FilePath
> pathFromDigest digest =
>   baseFilePath ++ (take 4 digest') ++ "/" ++ (drop 4 digest')

In order to return a `FilePath`, the digest must be unpacked from the
`ByteString`.

>   where digest' = BS.unpack digest

The `baseFilePath` value has the type `FilePath`, and contains the
root for where files should be stored in Nebula.

> baseFilePath :: FilePath
> baseFilePath = "/tmp/nebula/files/"

The other core function is `writeFile`, which takes a `ByteString` and
stores it in the appropriate path. It should return a `IO ()`
indicating that the file was successfully written.

> writeFile :: BS.ByteString -> IO ()
> writeFile content = do
>   createParents digest
>   BS.writeFile path content
>   where digest = digestHex content
>         path   = pathFromDigest digest

In order to derive the digest, there needs to be a function that
performs the SHA-256 hash of the content and returns the hex-encoded
form. It should take a `ByteString` and return a `ByteString`.

> digestHex :: BS.ByteString -> BS.ByteString
> digestHex content =
>   BS.pack . BS.unpack . Data.ByteString.Base16.encode $ SHA256.hash content

At this stage, attempts to write files will fail: the parent directory
will not exist. The `createParents` function will create the necessary
parent directories as needed.

> createParents :: BS.ByteString -> IO ()
> createParents digest =
>   Dir.createDirectoryIfMissing True $ parentFromDigest digest

Analogous to the `pathFromDigest` function is the `parentFromDigest`
function, which omits the final file name.

> parentFromDigest :: BS.ByteString -> FilePath
> parentFromDigest digest =
>   baseFilePath ++ (take 4 $ BS.unpack digest)


Looking skyward
---------------

With the two core functions completed, the base of Nebula is
done. This handles the interaction from receiving some data to
handling the file system flow. Now it is time to look up and figure
out how to get data to these functions.
