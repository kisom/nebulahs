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
      -- * Store context
      StoreContext
    , defaultContext

      -- * User API for dealing with entries
    , resolveTarget
    , uploadBlob
    , proxyEntry
) where

import qualified Data.ByteString.Char8 as BS

import qualified Nebula.Blob as Blob
import qualified Nebula.Entry as Entry
import qualified Nebula.Util as Util

{-
The file store contains the file store objects. A `StoreContext`
contains information about a store, such as what paths to store
things.
-}

-- |StoreContext contains the parent directories for the nebula store.
data StoreContext = StoreContext { storeBlobs   :: FilePath -- ^ path to blobs directory
                                 , storeEntries :: FilePath -- ^ path to entry storage
} deriving (Show, Read)

{- The default context takes place in the current working directory. -}

-- | defaultContext takes place in the current working directory; both
-- blobs and entries are stored in the same directory.
defaultContext :: StoreContext  -- ^ Returns a constant context.
defaultContext = StoreContext "nebula-store" "nebula-store"

-- | @resolveTarget@ takes a UUID identifying an entry, and attempts
-- to retrieve the blob it refers to.
resolveTarget :: String                      -- ^ Entry identifier
                 -> IO (Maybe BS.ByteString) -- ^ Contents of blob
resolveTarget uuid =
  if Util.isValidUUID uuid
  then do
    entry <- Entry.readEntry (storeEntries defaultContext) uuid
    case entry of
     Nothing    -> return Nothing
     Just entry -> do
       let target = Entry.target entry
       if Util.isValidUUID target then resolveTarget target
       else if Util.isValidHash target then Blob.readBlob (storeBlobs defaultContext) target
            else return Nothing
  else return Nothing


{- Uploading a blob requires that the blob be first written, and a new
   entry built from this. -}

uploadBlob :: BS.ByteString -> Maybe String -> IO (Maybe String)
uploadBlob blob p = do
  hashed <- Blob.writeBlob (storeBlobs defaultContext) blob
  entry  <- Entry.newEntry hashed p
  case entry of
   Nothing -> return Nothing
   Just e  -> do
     Entry.writeEntry (storeEntries defaultContext) (return e)
     return $ Just (Entry.entryID e)

-- | proxyEntry takes a UUID and creates a proxy for that entry.
proxyEntry :: String                -- ^ Identifier for entry to proxy
              -> IO (Maybe String)  -- ^ Identifier for the proxy
proxyEntry uuid = do
  if Util.isValidUUID uuid
    then do
      entry   <- Entry.readEntry (storeEntries defaultContext) uuid
      case entry of
       Nothing    -> return Nothing
       Just entry -> do
         proxied <- Entry.proxyEntry entry
         case proxied of
          Nothing       -> return Nothing
          Just proxied -> do
            _ <- Entry.writeEntry (storeEntries defaultContext) (return proxied)
            return . Just $ Entry.entryID proxied
    else return Nothing

writeProxied :: Entry.Entry -> IO String
writeProxied e = Entry.writeEntry (storeEntries defaultContext) (return e)

-- | @proxyAll@ produces a proxied lineage for the entry specified by
-- the UUID.
proxyAll :: String          -- ^ Identifier for entry to proxy
            -> IO [String]  -- ^ List of UUIDs for proxied entries
proxyAll uuid = do
  if Util.isValidUUID uuid
     then do
       entry <- Entry.readEntry (storeEntries defaultContext) uuid
       case entry of
        Nothing    -> return []
        Just entry -> do
          proxied <- Entry.proxyLineage (storeEntries defaultContext) entry
          mapM_ writeProxied proxied
          return $ map Entry.entryID proxied
    else return []
    
