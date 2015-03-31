> {-# LANGUAGE OverloadedStrings #-}
> module Nebula.Entry (
>   newEntry
> , writeEntry, readEntry
> , readParent, getLineage
> , proxyEntry, proxyLineage
>   ) where
>

> import qualified Data.Int as Int
> import Data.List.Split (splitOn)
> import qualified Data.UUID as UUID
> import Data.UUID.V4 (nextRandom)
> import qualified System.Directory as Dir
> import qualified System.FilePath as File
>
> import Nebula.Blob as Blob
> import Nebula.Util as Util

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

> data Entry = Entry { id      :: String
>                    , target  :: String
>                    , created :: Int.Int64
>                    , parent  :: Maybe String
> } deriving (Show, Read)

An entry is created as a reference to a blob identifier.

> createEntry :: String -> Maybe String -> IO Entry
> createEntry target parent = do
>   t <- Util.getTime
>   uuid <- nextRandom
>   return $ Entry (UUID.toString uuid) target t parent

The string parameters passed to createEntry must not be empty
strings. The target must be either a SHA-256 hash (i.e., it points to
a blob) or a UUID. The parent must either be Nothing or contain a
valid UUID, as the parent will always be an entry.

> validTarget :: String -> Bool
> validTarget s = (Util.isValidHash s) || (Util.isValidUUID s)

> validParent :: Maybe String -> Bool
> validParent Nothing = True
> validParent (Just s) = Util.isValidUUID s

> validEntryParams :: String -> Maybe String -> Bool
> validEntryParams t p = validTarget t && validParent p

> newEntry :: String -> Maybe String -> IO (Maybe Entry)
> newEntry t p = if validEntryParams t p
>                then do
>                  entry <- createEntry t p
>                  return $ Just entry
>                else return Nothing

Entries will be stored in some parent directory joined with a number
of path components built from the sections of the UUID. That is, the
UUID "8b00b986-b7cc-4493-acc8-ece4ff86092e" with a parent of "p" would
be split into "p/8b00b986/b7cc/4493/acc8/ece4ff86092e".

> entryPath :: FilePath -> Entry -> FilePath
> entryPath p (Entry id _ _ _) = idPath p id

> idPath :: FilePath -> String -> FilePath
> idPath p id = File.joinPath $ p : splitOn "-" id

An entry right now is just serialised as its show form.

> writeEntry :: FilePath -> IO Entry -> IO String
> writeEntry p e = do
>   entry <- e
>   let path = entryPath p entry
>   Dir.createDirectoryIfMissing True (File.takeDirectory path)
>   writeFile path (show entry)
>   return path

Reading an entry is a case of checking whether the entry exists, and
parsing the contents of the file.

> readEntry :: FilePath -> String -> IO (Maybe Entry)
> readEntry p id =
>   if validTarget id
>   then do
>     let path = idPath p id
>     entryExists <- Dir.doesFileExist path
>     if entryExists
>        then do
>          contents <- readFile path
>          return $ Just (read contents)
>       else return Nothing
>   else return Nothing

It's useful to be able to lookup the parent of an entry.

> readParent :: FilePath -> Maybe Entry -> IO (Maybe Entry)
> readParent p entry = case entry of
>    Just (Entry _ _ _ (Just parent)) -> readEntry p parent
>    _ -> return Nothing


An entry's lineage may be similarly fetched.

> getLineage :: FilePath -> String -> IO [String]
> getLineage p id = do
>   entry <- readEntry p id
>   case entry of
>    Just (Entry _ _ _ (Just parent)) -> do
>      parents <- getLineage p parent
>      return $! id : parents
>    Just (Entry _ _ _ Nothing) -> return [id]
>    _ -> return []

An entry may be proxied:

> proxyEntry :: Maybe Entry -> Maybe String -> IO (Maybe Entry)
> proxyEntry Nothing _ = return Nothing
> proxyEntry (Just (Entry id _ _ _)) parent = newEntry id parent

A whole lineage may be proxied:

> proxyLineage :: FilePath -> Maybe Entry -> IO [Entry]
> proxyLineage _ Nothing = return []
> proxyLineage p (Just (Entry id _ _ _)) = do
>   lineage <- getLineage p id
>   proxyAll (reverse lineage) Nothing

> proxyAll :: [String] -> Maybe String -> IO [Entry]
> proxyAll [] _ = return []
> proxyAll (t:ts) p = do
>   entry@(Entry id _ _ _) <- createEntry t p
>   parents <- proxyAll ts (Just id)
>   return . reverse $! entry : parents
