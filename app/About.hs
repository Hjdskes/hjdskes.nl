-- |
--Module: About
--Description: Generate the \"about me\" page.
--
--This module contains definitions for generating the \"about me\" page.
module About (
  buildAbout,
) where

import Config (Config (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks)
import Pandoc qualified (markdownToHTMLWithTemplateWithParams)
import System.FilePath ((-<.>), (</>))
import Text.Pandoc.Class (PandocMonad)

aboutPath :: FilePath
aboutPath = "about.md"

aboutTemplate :: FilePath
aboutTemplate = "templates/about.html"

aboutUrl :: FilePath
aboutUrl = aboutPath -<.> "html"

buildAbout :: MonadIO m => PandocMonad m => MonadReader Config m => m ()
buildAbout = do
  template <- asks ((</> aboutTemplate) . inputPath)
  source <- asks ((</> aboutPath) . inputPath)
  dest <- asks ((</> aboutUrl) . outputPath)
  globals <- asks siteParams
  Pandoc.markdownToHTMLWithTemplateWithParams globals template source dest
