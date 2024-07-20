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

indexHtmlOutputPath :: FilePath -> FilePath
indexHtmlOutputPath srcPath =
  outputDir </> Shake.dropExtension srcPath </> "index.html"

indexHtmlSourcePath :: FilePath -> FilePath
indexHtmlSourcePath =
  Shake.dropDirectory1
    . (<.> "md")
    . Shake.dropTrailingPathSeparator
    . Shake.dropFileName

typstToHtml :: FilePath -> Action Text
typstToHtml filePath = do
  content <- Shake.readFile' filePath
  Shake.quietly . Shake.traced "Typst to HTML" $ do
    doc <- runPandoc . Pandoc.readTypst readerOptions . T.pack $ content
    
    runPandoc . Pandoc.writeHtml5String writerOptions $ doc
  where
    readerOptions =
      Pandoc.def {Pandoc.readerExtensions = Pandoc.pandocExtensions}
    writerOptions =
      Pandoc.def {Pandoc.writerExtensions = Pandoc.pandocExtensions}

runPandoc action =
      Pandoc.runIO (Pandoc.setVerbosity Pandoc.ERROR >> action)
        >>= either (fail . show) return
