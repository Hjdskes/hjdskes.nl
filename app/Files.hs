-- |
--Module: Files
--Description: A module containing high level filesystem operations.
--
--This module contains definitions for working with files.
module Files (
  -- * File manipulation
  copyFile,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import System.Directory (Permissions (..))
import System.FilePath (takeDirectory)
import System.IO.Error (isPermissionError)
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist, getPermissions, removeFile, setPermissions)
import UnliftIO.Directory qualified as UnliftIO (copyFile)
import UnliftIO.Exception (catchIO, handleIO, tryIO)
import Prelude hiding (readFile, writeFile)

-- | Remove a file, but don't worry if it fails.
--   Imported from Shake's internal "General.Extra" module, but unlifted.
removeFile_ :: MonadUnliftIO m => FilePath -> m ()
removeFile_ path =
  removeFile path `catchIO` \e ->
    when (isPermissionError e) $
      handleIO (const $ pure ()) $ do
        perms <- getPermissions path
        setPermissions path perms {readable = True, searchable = True, writable = True}
        removeFile path

-- | Like @createDirectoryIfMissing True@ but faster, as it avoids
--   any work in the common case the directory already exists.
--   Imported from Shake's internal "General.Extra" module, but unlifted.
createDirectoryRecursive :: MonadUnliftIO m => FilePath -> m ()
createDirectoryRecursive dir = do
  exists <- tryIO $ doesDirectoryExist dir
  when (exists /= Right True) $ createDirectoryIfMissing True dir

-- | @copyFile old new@ copies the existing file from @old@ to @new@,
--   creating the new directory if necessary. Neither path may refer
--   to an existing directory.
copyFile :: MonadIO m => FilePath -> FilePath -> m ()
copyFile old new = liftIO $ do
  createDirectoryRecursive $ takeDirectory new
  removeFile_ new -- symlink safety
  UnliftIO.copyFile old new
