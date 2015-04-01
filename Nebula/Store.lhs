> {-# LANGUAGE OverloadedStrings #-}
> module Nebula.Store (
>   StoreContext
> , defaultContext
> ) where
>
> import qualified Nebula.Blob as Blob
> import qualified Nebula.Entry as Entry

The file store contains the file store objects. A `StoreContext`
contains information about a store, such as what paths to store
things.

> data StoreContext = StoreContext { storeFiles   :: FilePath -- path to file entries directory
>                                  , storeEntries :: FilePath -- path to entry storage
> } deriving (Show, Read)

The default context takes place in the current working directory.

> defaultContext :: StoreContext
> defaultContext = StoreContext "nebula-store" "nebula-store"

Uploading a blob requires that the blob be 
