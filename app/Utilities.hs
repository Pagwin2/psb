module Utilities where

import Data.Text (Text)
import Development.Shake.FilePath ((<.>), (</>))
import qualified Data.Text as T
import Data.Yaml.Aeson
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as Shake
import qualified Text.Pandoc as Pandoc
import Config
import Development.Shake (Action)
import Types
import Data.Maybe (fromMaybe)
import Data.Time

indexHtmlOutputPath :: FilePath -> FilePath
indexHtmlOutputPath srcPath =
  outputDir </> Shake.dropExtension srcPath </> "index.html"

indexHtmlSourcePath :: FilePath -> FilePath
indexHtmlSourcePath =
   Shake.dropDirectory1
    . (<.> "typ")
    . Shake.dropTrailingPathSeparator
    . Shake.dropFileName

indexHtmlMetaPath :: FilePath -> FilePath
indexHtmlMetaPath = typstMetaPath . indexHtmlSourcePath

typstMetaPath :: FilePath -> FilePath
typstMetaPath typstPath = Shake.dropExtension typstPath <.> "yaml"


typstToHtml :: FilePath -> Action Text
typstToHtml filePath = do
  content <- Shake.readFile' filePath
  Shake.quietly . Shake.traced "Typst to HTML" $ do
    doc <- runPandoc . Pandoc.readTypst readerOptions . T.pack $ content
    html <- runPandoc . Pandoc.writeHtml5String writerOptions $ doc
    return html
  where
    readerOptions =
      Pandoc.def {Pandoc.readerExtensions = Pandoc.pandocExtensions}
    writerOptions =
      Pandoc.def {Pandoc.writerExtensions = Pandoc.pandocExtensions}
    runPandoc action =
          Pandoc.runIO (Pandoc.setVerbosity Pandoc.ERROR >> action)
            >>= either (fail . show) return

yamlToPost :: FilePath -> Action Post
yamlToPost path = do
    post <- decodeFileThrow path
    let post' = dateTransform post
    return $ fromMaybe post post'
    where
    dateTransform post@(Post{postDate}) = do
        postDate' <- postDate
        let postDate'' =  dateStrTransform $ T.unpack postDate'
        Just post {
            postDate = postDate''
        }
    dateStrTransform date = do 
        date' <- parseTimeM False defaultTimeLocale "%Y-%-m-%-d" date
        Just $ T.pack $ formatTime @UTCTime defaultTimeLocale "%b %e, %Y" date'
