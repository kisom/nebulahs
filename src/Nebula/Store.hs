{-# LANGUAGE OverloadedStrings #-}
{-|

   The functions in this module should mimic those in the Clojure
   version. This means the following functions should be provided:

     - delete-entry
     - entry-history
     - entry-info : entry-info returns a JSON string containing
       information about an entry. This will filter out the target
       and parent entries, as users should not see this information.
     - load-history
     - proxy-all
     - proxy-entry
     - resolve-target : resolve-target takes a UUID, and attempts to
       retrieve the blob it refers to.
     - upload-blob : upload-blob attempts to store a blob, returning
       the UUID for the new entry created for it.
     - validate-id
 -}

module Nebula.Store
    (
      -- * Functions for dealing with a store's context.
      StoreContext
    , defaultContext
) where

import qualified Data.ByteString.Char8 as BS

import qualified Nebula.Blob as Blob
import qualified Nebula.Entry as Entry

{-
The file store contains the file store objects. A `StoreContext`
contains information about a store, such as what paths to store
things.
-}

-- |StoreContext contains the parent directories for the nebula store.
data StoreContext = StoreContext { storeFiles   :: FilePath -- ^ path to file entries directory
                                 , storeEntries :: FilePath -- ^ path to entry storage
} deriving (Show, Read)

{- The default context takes place in the current working directory. -}

-- | defaultContext takes place in the current working directory; both
-- blobs and entries are stored in the same directory.
defaultContext :: StoreContext  -- ^ Returns a constant context.
defaultContext = StoreContext "nebula-store" "nebula-store"

{- Uploading a blob requires that the blob be first written, and a new
   entry built from this. -}

-- > uploadBlob :: BS.ByteString -> Maybe String -> IO (Maybe String)
-- > uploadBlob blob p = do
-- >   hashed <- Blob.writeBlob (defaultContext 
