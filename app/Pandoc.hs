-- |
--Module: Pandoc
--Description: A module containing high level functions working with Pandoc.
--
--This module contains definitions for generating HTML pages from templates and source files written in Markdown.
module Pandoc (
  markdownToHTML,
  markdownToHTML',
  markdownToHTMLWithTemplate,
  markdownToHTMLWithTemplate',
  markdownToHTMLWithTemplateWithParams,
  markdownToHTMLWithTemplateWithParams',
) where

import Control.Monad.Error.Class (liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON (..)) --, object, (.=))
import Data.Default (def)
import Data.Text (Text)
import Files qualified (readFile, writeFile)
import Text.DocTemplates (toContext)
import Text.Pandoc (Pandoc)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Extensions (Extension (..), extensionsFromList, githubMarkdownExtensions)
import Text.Pandoc.Highlighting (tango)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))
import Text.Pandoc.Readers qualified as Pandoc (readMarkdown)
import Text.Pandoc.Shared (mapLeft)
import Text.Pandoc.Templates (Template)
import Text.Pandoc.Templates qualified as Pandoc (compileTemplate, getTemplate, runWithPartials)
import Text.Pandoc.Writers qualified as Pandoc (writeHtml5String)
import Text.Pandoc.Writers.Shared qualified as Pandoc (addVariablesToContext)

-- | Reasonable options for reading a Markdown file.
-- Behaves similar to Github Flavoured Markdown.
defaultMarkdownOptions :: ReaderOptions
defaultMarkdownOptions =
  def {readerExtensions = extensions}
  where
    extensions =
      mconcat
        [ extensionsFromList
            [ Ext_yaml_metadata_block
            , Ext_fenced_code_attributes
            , Ext_auto_identifiers
            ]
        , githubMarkdownExtensions
        ]

-- | Reasonable options for rendering to HTML.
-- Includes default code highlighting rules.
defaultHtml5Options :: WriterOptions
defaultHtml5Options =
  def
    { writerHighlightStyle = Just tango
    , writerExtensions = writerExtensions def
    }

-- | Loads a template from the given path and compiles it, including any partials it references.
-- The partials, if any, have to be relative to the path of the template.
applyTemplate :: MonadIO m => PandocMonad m => FilePath -> m (Template Text)
applyTemplate path = do
  contents <- Pandoc.getTemplate path
  eitherTemplate <- Pandoc.runWithPartials (Pandoc.compileTemplate path contents)
  liftIO . liftEither . mapLeft userError $ eitherTemplate

-- | Convert Markdown to HTML using 'defaultMarkdownOptions' and 'defaultHtml5Options'.
--   Reads the Markdown from the source path and writes the HTML to the destination path.
markdownToHTML :: MonadIO m => PandocMonad m => FilePath -> FilePath -> m ()
markdownToHTML markdownPath htmlPath = Files.readFile markdownPath >>= markdownToHTML' >>= Files.writeFile htmlPath

-- | Convert Markdown to HTML using 'defaultMarkdownOptions' and 'defaultHtml5Options'.
markdownToHTML' :: PandocMonad m => Text -> m Text
markdownToHTML' = markdownToHTMLWithOpts defaultMarkdownOptions defaultHtml5Options

-- | Convert Markdown to HTML using 'defaultMarkdownOptions' and 'defaultHtml5Options', using the given 'Template'.
--   Reads the Markdown from the source path and writes the HTML to the destination path.
markdownToHTMLWithTemplate :: MonadIO m => PandocMonad m => FilePath -> FilePath -> FilePath -> m ()
markdownToHTMLWithTemplate templatePath markdownPath htmlPath = do
  template <- applyTemplate templatePath
  markdown <- Files.readFile markdownPath
  html <- markdownToHTMLWithTemplate' template markdown
  Files.writeFile htmlPath html

-- | Convert Markdown to HTML using 'defaultMarkdownOptions' and 'defaultHtml5Options', using the given 'Template'.
markdownToHTMLWithTemplate' :: PandocMonad m => Template Text -> Text -> m Text
markdownToHTMLWithTemplate' template =
  markdownToHTMLWithOpts defaultMarkdownOptions (defaultHtml5Options {writerTemplate = Just template})

-- | Convert Markdown to HTML using 'defaultMarkdownOptions' and 'defaultHtml5Options', using the given 'Template'
--   and inserting the given additional variables. Reads the Markdown from the source path and writes the HTML to
--   the destination path.
markdownToHTMLWithTemplateWithParams :: MonadIO m => PandocMonad m => ToJSON a => a -> FilePath -> FilePath -> FilePath -> m ()
markdownToHTMLWithTemplateWithParams params templatePath markdownPath htmlPath = do
  template <- applyTemplate templatePath
  markdown <- Files.readFile markdownPath
  html <- markdownToHTMLWithTemplateWithParams' params template markdown
  Files.writeFile htmlPath html

-- | Convert Markdown to HTML using 'defaultMarkdownOptions' and 'defaultHtml5Options', using the given 'Template'
--   and inserting the given additional variables.
markdownToHTMLWithTemplateWithParams' :: PandocMonad m => ToJSON a => a -> Template Text -> Text -> m Text
markdownToHTMLWithTemplateWithParams' params template =
  markdownToHTMLWithOpts defaultMarkdownOptions (wops {writerVariables = variables})
  where
    wops = defaultHtml5Options {writerTemplate = Just template}
    variables = Pandoc.addVariablesToContext wops (toContext $ toJSON params)

-- | Convert Markdown to HTML.
markdownToHTMLWithOpts ::
  PandocMonad m =>
  -- | Pandoc reader options to specify extensions or other functionality.
  ReaderOptions ->
  -- | Pandoc writer options to modify output.
  WriterOptions ->
  -- | Markdown source text for conversion.
  Text ->
  m Text
markdownToHTMLWithOpts rops wops =
  pipe (Pandoc.readMarkdown rops) (Pandoc.writeHtml5String wops)

-- | A Pandoc reader is a function loading the 'Pandoc' document from a value.
type PandocReader m a = a -> m Pandoc

-- | A Pandoc writer is a function rendering the 'Pandoc' documented to a value.
type PandocWriter m a = Pandoc -> m a

-- | Load in a source document using the given 'PandocReader', then render the 'Pandoc'
--   using the given 'PandocWriter'.
pipe ::
  PandocMonad m =>
  -- | The reader used to load the document.
  PandocReader m a ->
  -- | The writer used to render the document including its metadata.
  PandocWriter m b ->
  -- | The source document.
  a ->
  -- | The rendered document, including its metadata.
  m b
pipe reader writer content = reader content >>= writer
