module Utilities.Action where

import Config (postGlobs)
import Control.Monad (filterM)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Yaml.Aeson
import Development.Shake (Action)
import qualified Development.Shake as Shake
import HTML
import Markdown
import Text.Megaparsec (errorBundlePretty, runParserT)
import Types

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

now :: Action T.Text
now = Shake.liftIO $ fmap (T.pack . iso8601Show) getCurrentTime

isDraft' :: [(FilePath -> Bool, FilePath -> Action Post)] -> FilePath -> Action Bool
isDraft' postHandles path = do
  let action =
        case find (\(test, _) -> test path) postHandles of
          (Just (_, action')) -> action'
          Nothing -> error "no post handle for this file type"
  post <- action path
  pure $ case postDraft post of
    Just ret -> ret
    Nothing -> (error $ "Missing draft attr: " ++ path)

getPublishedPosts :: (FilePath -> Action Bool) -> Action [FilePath]
getPublishedPosts draftCheck = do
  postPaths <- Shake.getDirectoryFiles "" postGlobs
  filterM (fmap not . draftCheck) postPaths
