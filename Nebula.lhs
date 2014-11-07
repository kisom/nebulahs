> import Crypto.Hash.SHA256 as SHA256
> import Data.ByteString

Introduction
------------

Nebula is a toy file storage system written in Literate Haskell.

I need a project to play around with the practicals of Haskell. To me,
practical uses are why I choose a given programming language mostly,
and in this case, what I want to do intersects my systems and
networking background. This idea is inspired by a talk from Joe
Armstrong, from some previous ideas ve hacked on, and from talking on
the tyrfingr IRC channel. 

The basic idea is a file storage system, available via a network
connection. Files are identified by a SHA256 hash, preferably with
some metadata associated (such as creation time). A file is immutable;
once entered, it doesn't change.


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
>    putStrLn "Les nuages noirs Font la mer noire."

> fileHash :: FilePath -> Maybe ByteString
> fileHash path = do
>     contents <- readFile path
>     putStrLn "read file"
