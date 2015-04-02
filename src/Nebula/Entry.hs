{-# LANGUAGE OverloadedStrings #-}

{-|

This module provides the API that is exposed to users; it is based
around the 'Entry' type.

Users directly interacting with blobs presents two problems:

Information leakage: if Alice wants to determine if someone already
has a copy of some data, she attempts to read its SHA-256 digest. The
server will return the data if she has it. This data is of no
consequence to Alice, as she likely already had a copy of the data to
produce the hash.

Managing data is more difficult: in the case where a user asks the
server to delete a file that multiple users have, the server has no
way to determine what other users might have the data. One user can
then remove data that other users may still wish to
retain. Alternatively, the server might refuse to delete this data,
which means users have no way to remove data from the system.

The solution is to only access blob IDs in the software, and to
provide users with UUIDs to reference their data. A UUID contains

- the ID

- the referenced object ID (see below, this may be a SHA-256 ID or
  another UUID)

- creation date

- the parent UUID
- UUIDs of any children

In order to provide useful access control, a reference may be a proxy
reference: that is, it may refer to another blob reference. This means
that a user can grant revocable access to the data without
jeopardizing their own access.

Therefore, to know an ID is to have access to that ID. For this
reason, users can only see the metadata and none of the IDs. The
system needs an API that can traverse the history tree without
exposing these IDs to users. Proxy objects either need to be presented
with no history information (empty parent and children), or the entire
history needs to be proxied. Similarly, a revocation API needs to be
able to take into account that the entire history tree may be proxied.

This data must be stored in a persistent data structure, such as a
database.

This reference is named an entry. This reference is the only interface
users have with blobs.

-}
module Nebula.Entry
    (
      -- * Entry accessors
      Entry
    , newEntry
    , entryID
    , target
      
      -- * Entry persistence
    , writeEntry
    , readEntry

      -- * Lineage
    , getLineage

      -- * Entry proxying

      -- | Proxying is a means of sharing a reference to an entry
      -- without sharing the reference directly.
    , proxyEntry, proxyLineage
  ) where

import qualified Data.Int as Int
import Data.List.Split (splitOn)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import qualified System.Directory as Dir
import qualified System.FilePath as File

import Nebula.Blob as Blob
import Nebula.Util as Util

-- | The core type of this package is the Entry: a reference to either
-- a blob or an entry.
data Entry = Entry
             { -- | A UUID identifying this entry.
               entryID :: !String
               
               -- | What 'Entry' or blob does this refer to?
             , target  :: !String

               -- | Unix timestamp (at millisecond or better
               -- resolution) of when the entry was created.
             , created :: !Int.Int64
               
               -- | An optional parent. It's possible this is the
               -- first entry in its lineage, in which case it will be
               -- @Nothing@. If it's @Just@ anything, it will be a
               -- UUID pointing to another Entry. This field will
               -- never point to a blob.
             , parent  :: !(Maybe String)
} deriving (Show, Read)

-- | createEntry is the raw function for creating a new entry. It does
-- no validation of its parameters, and returns a new timestamped
-- entry with a random ID.
createEntry :: String        -- ^ Target identifier
            -> Maybe String  -- ^ Entry's parent (what entry precedes this one?)
            -> IO Entry      -- ^ The timestamped entry
createEntry target parent = do
  t <- Util.getTime
  uuid <- nextRandom
  return $ Entry (UUID.toString uuid) target t parent

{-
   The string parameters passed to createEntry must not be empty
   strings. The target must be either a SHA-256 hash (i.e., it points to
   a blob) or a UUID. The parent must either be Nothing or contain a
   valid UUID, as the parent will always be an entry.
 -}

validTarget :: String -> Bool
validTarget s = (Util.isValidHash s) || (Util.isValidUUID s)

validParent :: Maybe String -> Bool
validParent Nothing = True
validParent (Just s) = Util.isValidUUID s

validEntryParams :: String -> Maybe String -> Bool
validEntryParams t p = validTarget t && validParent p

-- | newEntry takes a target identifier that indicates what this entry
-- should point to, and an an optional string containing the parent
-- entry; if these two identifiers are valid, a new entry will be
-- created with a random identifier.
--
-- A valid target identifier may be either
--
--  * a blob identifier: if the entry points directly at a file entry,
--  the identifier will be a valid blob identifier.
--
--  * an entry identifier: if the entry is a proxied entry, the
--  identifier will be a valid entry identifier.
newEntry :: String               -- ^ Target identifier
            -> Maybe String      -- ^ Optional parent entry
            -> IO (Maybe Entry)  -- ^ Resulting entry
newEntry t p = if validEntryParams t p
               then do
                 entry <- createEntry t p
                 return $ Just entry
               else return Nothing

{-
Entries will be stored in some parent directory joined with a number
of path components built from the sections of the UUID. That is, the
UUID "8b00b986-b7cc-4493-acc8-ece4ff86092e" with a parent of "p" would
be split into "p/8b00b986/b7cc/4493/acc8/ece4ff86092e".
-}

entryPath :: FilePath -> Entry -> FilePath
entryPath p (Entry id _ _ _) = idPath p id

idPath :: FilePath -> String -> FilePath
idPath p id = File.joinPath $ p : splitOn "-" id

{- An entry right now is just serialised as its show form. -}

-- | writeEntry serialises an entry and stores it in an appropriate
-- subdirectory under the given parent directory.
writeEntry :: FilePath      -- ^ Parent directory
              -> IO Entry   -- ^ Entry to store
              -> IO String  -- ^ Path to stored entry
writeEntry p e = do
  entry <- e
  let path = entryPath p entry
  Dir.createDirectoryIfMissing True (File.takeDirectory path)
  writeFile path (show entry)
  return path

{- Reading an entry is a case of checking whether the entry exists, and
   parsing the contents of the file. -}

-- | readEntry looks up the given identifier in the given top-level
-- directory, returned the 'Entry' if it exists.
readEntry :: FilePath             -- ^ Parent directory containing the entries
             -> String            -- ^ Entry identifier
             -> IO (Maybe Entry)  -- ^ Stored entry
readEntry p id =
  if validTarget id
  then do
    let path = idPath p id
    entryExists <- Dir.doesFileExist path
    if entryExists
       then do
         contents <- readFile path
         return $ Just (read contents)
      else return Nothing
  else return Nothing

{- It's useful to be able to lookup the parent of an entry. -}

readParent :: FilePath -> Maybe Entry -> IO (Maybe Entry)
readParent p entry = case entry of
   Just (Entry _ _ _ (Just parent)) -> readEntry p parent
   _ -> return Nothing


{- An entry's lineage may be similarly fetched. -}

-- | getLineage returns a list of identifiers pointing to an entry's
-- lineage. The list will have the root identifier at the list's tail
-- position.
getLineage :: FilePath        -- ^ Parent directory containing entries
              -> String       -- ^ Entry identifier
              -> IO [String]  -- ^ Entry's lineage
getLineage p id = do
  entry <- readEntry p id
  case entry of
   Just (Entry _ _ _ (Just parent)) -> do
     parents <- getLineage p parent
     return $! id : parents
   Just (Entry _ _ _ Nothing) -> return [id]
   _ -> return []

{- An entry may be proxied: -}

-- | proxyEntry creates a reference to the entry, removing any history
-- from it. The resulting entry will have no reference to a parent.
proxyEntry :: Entry               -- ^ Entry to proxy
           -> IO (Maybe Entry)    -- ^ Proxied entry
proxyEntry (Entry id _ _ _) = newEntry id Nothing

proxy :: Maybe Entry -> Maybe String -> IO (Maybe Entry)
proxy Nothing _ = return Nothing


{- A whole lineage may be proxied: -}

-- | proxyLineage creates a proxy of an entire lineage. It will return a
-- list of entries, with each each entry proxied to a corresponding
-- entry in the original lineage.
proxyLineage :: FilePath        -- ^ Parent directory containing entries
                -> Entry        -- ^ Entry whose lineage should be proxied
                -> IO [Entry]   -- ^ Proxied lineage
proxyLineage p (Entry id _ _ _) = do
  lineage <- getLineage p id
  proxied <- proxyAll (reverse lineage) Nothing
  return $ reverse proxied

proxyAll :: [String] -> Maybe String -> IO [Entry]
proxyAll [] _ = return []
proxyAll (t:ts) p = do
  entry@(Entry id _ _ _) <- createEntry t p
  parents <- proxyAll ts (Just id)
  return $! entry : parents
