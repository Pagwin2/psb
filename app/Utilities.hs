{-# LANGUAGE ApplicativeDo, DataKinds, DeriveGeneric #-}
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

indexHtmlOutputPath :: FilePath -> FilePath
indexHtmlOutputPath srcPath =
  outputDir </> Shake.dropExtension srcPath </> "index.html"

indexHtmlSourcePath :: FilePath -> FilePath
indexHtmlSourcePath =
  Shake.dropDirectory1
    . (<.> "typ")
    . Shake.dropTrailingPathSeparator
    . Shake.dropFileName

typstToHtml :: FromJSON a => FilePath -> Action (a, Text)
typstToHtml filePath = do
  content <- Shake.readFile' filePath
  Shake.quietly . Shake.traced "Typst to HTML" $ do
    doc@(Pandoc meta _) <- runPandoc . Pandoc.readTypst readerOptions . T.pack $ content
    meta' <- fromMeta meta
    html <- runPandoc . Pandoc.writeHtml5String writerOptions $ doc
    return (meta', html)
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
