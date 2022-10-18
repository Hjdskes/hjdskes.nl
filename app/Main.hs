module Main where

import About (buildAbout)
import Config (Config)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, runReaderT)
import Data.Yaml (decodeFileThrow)
import Static (copyStaticFiles)
import Text.Pandoc.Class (PandocMonad, runIOorExplode, setUserDataDir, setVerbosity)
import Text.Pandoc.Logging (Verbosity (..))

buildRules :: MonadIO m => PandocMonad m => MonadReader Config m => m ()
buildRules = do
  copyStaticFiles
  buildAbout

main :: IO ()
main = do
  -- TODO: pass this as a parameter
  config <- decodeFileThrow "/home/jente/src/new-website/site/config.yaml"
  runIOorExplode $ do
    setVerbosity INFO
    setUserDataDir Nothing
    runReaderT buildRules config
