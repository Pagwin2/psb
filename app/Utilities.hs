{-# LANGUAGE ApplicativeDo, DataKinds, NamedFieldPuns #-}
{-# LANGUAGE DerivingVia, LambdaCase, TypeApplications #-}
module Utilities where

import Data.Text (Text)
import Development.Shake.FilePath ((<.>), (</>))
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as Shake
import qualified Text.Pandoc as Pandoc
import Config
import Development.Shake (Action)
import Text.Pandoc 
import Data.Aeson as A
import Data.Time (UTCTime(UTCTime), formatTime, defaultTimeLocale, parseTimeM)
import Types
import Data.Maybe (fromJust)

indexHtmlOutputPath :: FilePath -> FilePath
indexHtmlOutputPath srcPath =
  outputDir </> Shake.dropExtension srcPath </> "index.html"

indexHtmlSourcePath :: FilePath -> FilePath
indexHtmlSourcePath =
   Shake.dropDirectory1
    . (<.> "typ")
    . Shake.dropTrailingPathSeparator
    . Shake.dropFileName

typstToHtml :: FilePath -> Action (Post, Text)
typstToHtml filePath = do
  content <- Shake.readFile' filePath
  Shake.quietly . Shake.traced "Typst to HTML" $ do
    doc@(Pandoc meta _) <- runPandoc . Pandoc.readTypst readerOptions . T.pack $ content
    meta' <- fromMeta meta
    let dateTransformedMeta = dateTransform meta'
    html <- runPandoc . Pandoc.writeHtml5String writerOptions $ doc
    return (fromJust dateTransformedMeta, html)
  where
    readerOptions =
      Pandoc.def {Pandoc.readerExtensions = Pandoc.pandocExtensions}
    writerOptions =
      Pandoc.def {Pandoc.writerExtensions = Pandoc.pandocExtensions}
    fromMeta (Meta meta) =
          A.fromJSON . A.toJSON <$> traverse metaValueToJSON meta >>= \case
            Success res -> pure res
            Error err -> fail $ "json conversion error:" <> err
    metaValueToJSON = \case
          MetaMap m -> A.toJSON <$> traverse metaValueToJSON m
          MetaList m -> A.toJSONList <$> traverse metaValueToJSON m
          MetaBool m -> pure $ A.toJSON m
          MetaString m -> pure $ A.toJSON $ T.strip m
          MetaInlines m -> metaValueToJSON $ MetaBlocks [Plain m]
          MetaBlocks m ->
            fmap (A.toJSON . T.strip)
              . runPandoc
              . Pandoc.writePlain Pandoc.def
              $ Pandoc mempty m
    runPandoc action =
          Pandoc.runIO (Pandoc.setVerbosity Pandoc.ERROR >> action)
            >>= either (fail . show) return
    dateTransform post@(Post{postDate}) = do
        postDate' <- dateStrTransform $ T.unpack $ fromJust postDate
        Just post {
            postDate = Just postDate'
        }
    dateStrTransform date = do 
        date' <- parseTimeM False defaultTimeLocale "%Y-%-m-%-d" date
        Just $ T.pack $ formatTime @UTCTime defaultTimeLocale "%b %e, %Y" date'
