module Utilities.FilePath where

import Config
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake.FilePath ((<.>), (</>))
import qualified Development.Shake.FilePath as FP

indexHtmlOutputPath :: FilePath -> FilePath
indexHtmlOutputPath srcPath =
  outputDir </> FP.dropExtension srcPath </> "index.html"

indexHtmlSourcePaths :: FilePath -> [FilePath]
indexHtmlSourcePaths path = map ($ path) [indexHtmlMarkdownSourcePath]

indexHtmlMarkdownSourcePath :: FilePath -> FilePath
indexHtmlMarkdownSourcePath =
  FP.dropDirectory1
    . (<.> "md")
    . FP.dropTrailingPathSeparator
    . FP.dropFileName

isMarkdownPost :: FilePath -> Bool
isMarkdownPost path = FP.takeExtension path == ".md"

urlConvert :: FilePath -> Text
urlConvert = T.pack . FP.dropFileName . flip FP.replaceDirectory1 "https://pagwin.xyz"
