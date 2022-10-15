-- |
--Module: Config
--Description: A module containing definitions for configuration.
--
--This module contains definitions for generator configuration that will be used when generating the website.
module Config (
  -- * Generator configuration
  Config (..),
) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

-- | The entire configuration needed to generate a website.
--   This is typically read from a configuration file.
data Config = Config
  { -- | The path from which the files to compile will be read.
    inputPath :: FilePath
  , -- | The path into which the website will be generated.
    outputPath :: FilePath
  }
  deriving (Generic, Eq, Ord, Show, FromJSON)
