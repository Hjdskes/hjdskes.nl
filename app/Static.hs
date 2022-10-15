-- |
--Module: Static
--Description: Copy static files (CSS, Javascript, images, et cetera) from the input path to the output path.
--
--This module contains definitions for copying static files (CSS, Javascript, images, et cetera) from the input
--path to the output path.
module Static (
  copyStaticFiles,
) where

import Config (Config (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Functor (void)
import Data.Traversable (forM)
import Files (copyFile)
import System.FilePath ((</>))
import System.FilePattern.Directory (getDirectoryFiles)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Logging (LogMessage (..))

-- | Copy all static files to their destination.
copyStaticFiles :: MonadIO m => PandocMonad m => MonadReader Config m => m ()
copyStaticFiles = do
  staticPath <- asks ((</> "static") . inputPath)
  outputPath' <- asks outputPath
  -- TODO: can this be implemented in terms of PandocMonad's glob?
  filePaths <- liftIO $ getDirectoryFiles staticPath ["**/*"]
  void . forM filePaths $ \filePath -> do
    let source = staticPath </> filePath
        target = outputPath' </> filePath
    report $ LoadedResource target source
    copyFile source target
