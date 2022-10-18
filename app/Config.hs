-- |
--Module: Config
--Description: A module containing definitions for configuration.
--
--This module contains definitions for generator and site-wide parameters that will be used
--when substituting the templates.
module Config (
  -- * Generator configuration
  Config (..),

  -- * Global, site-wide parameters
  SiteParams (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Deriving.Aeson (CustomJSON (..), OmitNothingFields)
import GHC.Generics (Generic)

-- | The entire configuration needed to generate a website.
-- This is typically read from a configuration file.
data Config = Config
  { -- | The path from which the files to compile will be read.
    inputPath :: FilePath
  , -- | The path into which the website will be generated.
    outputPath :: FilePath
  , -- | The global, site-specific parameters.
    siteParams :: SiteParams
  }
  deriving (Generic, Eq, Ord, Show, FromJSON)

-- | Global, site-wide parameters that should be available on any template.
data SiteParams = SiteParams
  { -- | The author of the website.
    author :: String
  , -- | The base URL of the website, e.g. \"https://www.example.com\". This is used to make some references absolute.
    baseUrl :: String
  , -- | The language code of the website, e.g. in HTML's \"@lang@\" property.
    lang :: String
  , -- | The copyright string, e.g. \"Copyright © 2021-2022\".
    copyright :: String
  , -- | The title of the website displayed on links to the website, e.g. \"@og:site_name@\".
    siteTitle :: String
  , -- | The path to the logo displayed on links to the website, e.g. \"@og:image@\".
    siteLogo :: FilePath
  , -- | Your email address for contact.
    emailAddress :: Maybe String
  , -- | Your Twitter handle for contact, without the \"@\" symbol.
    twitterHandle :: Maybe String
  , -- | Your Github username for contact.
    githubUser :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show)
  deriving (FromJSON, ToJSON) via CustomJSON '[OmitNothingFields] SiteParams
