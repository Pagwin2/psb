module Utilities where

import Config
import Control.Monad (filterM)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Yaml.Aeson
import Development.Shake (Action)
import qualified Development.Shake as Shake
import Development.Shake.FilePath ((<.>), (</>))
import qualified Development.Shake.FilePath as FP
import HTML
import Markdown
import Text.Megaparsec (errorBundlePretty, runParserT)
import Types

indexHtmlOutputPath :: FilePath -> FilePath
indexHtmlOutputPath srcPath =
  outputDir </> FP.dropExtension srcPath </> "index.html"

-- were applicative shenanigans necessary? no
-- but using them felt cool
indexHtmlSourcePaths :: FilePath -> [FilePath]
indexHtmlSourcePaths path = [indexHtmlMarkdownSourcePath] <*> [path]

indexHtmlMarkdownSourcePath :: FilePath -> FilePath
indexHtmlMarkdownSourcePath =
  FP.dropDirectory1
    . (<.> "md")
    . FP.dropTrailingPathSeparator
    . FP.dropFileName

markdownToHtml :: (FromJSON a) => FilePath -> Action (a, Text)
markdownToHtml filePath = do
  content <- Shake.readFile' filePath
  let parse = runIdentity $ runParserT (liftA2 (,) Markdown.metadata Markdown.document) filePath content
  let (metadataText, document) = case parse of
        Right (a, b) -> (a, b)
        Left e -> error $ errorBundlePretty e

  let metadata = case decodeEither' $ encodeUtf8 metadataText of
        Right m -> m
        Left e -> error $ show e
  pure (metadata, compileToHTML document)

now :: Action T.Text
now = Shake.liftIO $ fmap (T.pack . iso8601Show) getCurrentTime

markdownToPost :: FilePath -> Action Post
markdownToPost path = do
  content <- Shake.readFile' path
  let parse = runIdentity $ runParserT Markdown.metadata path content
  let postData = case parse of
        Right p -> p
        Left e -> error $ errorBundlePretty e
  let post = case decodeEither' $ encodeUtf8 postData of
        Right p -> p
        Left e -> error $ show e
  pure post

yamlToPost :: FilePath -> Action Post
yamlToPost path = do
  post <- decodeFileThrow path
  -- let post' = dateTransform post
  return post

isMarkdownPost :: FilePath -> Bool
isMarkdownPost path = FP.takeExtension path == ".md"

postHandles :: [(FilePath -> Bool, FilePath -> Action Post)]
postHandles = [(isMarkdownPost, markdownToPost)]

isDraft :: FilePath -> Action Bool
isDraft path = do
  let action =
        case find (\(test, _) -> test path) postHandles of
          (Just (_, action')) -> action'
          Nothing -> error "no post handle for this file type"
  post <- action path
  return $ case postDraft post of
    Just ret -> ret
    Nothing -> (error $ "Missing draft attr: " ++ path)

getPublishedPosts :: Action [FilePath]
getPublishedPosts = do
  postPaths <- Shake.getDirectoryFiles "" postGlobs
  filterM (fmap not . isDraft) postPaths

parseDate :: Text -> Maybe Text
parseDate str = do
  date <- parseTimeM False defaultTimeLocale "%Y-%-m-%-d" $ T.unpack str
  -- need to append the time to avoid potential issues
  return $ T.pack $ formatTime @UTCTime defaultTimeLocale "%Y-%m-%dT00:00:00Z" date

urlConvert :: FilePath -> Text
urlConvert = T.pack . FP.dropFileName . flip FP.replaceDirectory1 "https://pagwin.xyz"
