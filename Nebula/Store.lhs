> module Nebula.Store (
>     File, fileID, filePath, fileOwner 
>   , Identifier
>   , Owner
> )
> where

The file store contains the file store objects. A `StoreContext`
contains information about a store, such as what paths to store
things.

> data StoreContext = StoreContext {
>   storeFiles   :: FilePath -- path to file entries directory
> , storeMeta    :: FilePath -- path to metadata directory
> , storeEntries :: FilePath -- path to entry storage
> }

An `Identifier` is a string that contains a unique identifier for an
entry.

> type Identifier = String

An `Owner` is a unique identifier indicating ownership of a file or
entry.

> type Owner = String

A file represents a named revision of a content blob.

> data File = File { fileID :: Identifier
>                  , filePath :: FilePath
>                  , fileOwner :: Identifier
> }

