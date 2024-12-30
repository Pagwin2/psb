module Templates where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import qualified Development.Shake as Shake
import Development.Shake.FilePath ((</>))
import GHC.Stack (HasCallStack)
import qualified Text.Mustache as Mus
import qualified Text.Mustache.Compile as Mus
import Types (Post (postAuthor, postContent, postDate, postDescription, postLink, postTags, postTitle), RenderedPost (RenderedPost, rPostAuthor, rPostContent, rPostDate, rPostHasTags, rPostId, rPostIsoDate, rPostLink, rPostSummary, rPostTags, rPostTitle))
import Utilities

applyTemplate :: (HasCallStack, (ToJSON a)) => String -> a -> Action Text
applyTemplate templateName context = do
  tmpl <- readTemplate $ "templates" </> templateName
  -- liftIO $ print $ A.toJSON context
  case Mus.checkedSubstitute tmpl (A.toJSON context) of
    ([], text) -> return text
    (errs, _) ->
      error $
        "Error while substituting template "
          <> templateName
          <> ": "
          <> unlines (map show errs)

applyTemplateAndWrite :: (ToJSON a) => String -> a -> FilePath -> Action ()
applyTemplateAndWrite templateName context outputPath =
  applyTemplate templateName context
    >>= Shake.writeFile' outputPath . T.unpack

readTemplate :: FilePath -> Action Mus.Template
readTemplate templatePath = do
  Shake.need [templatePath]
  eTemplate <-
    Shake.quietly
      . Shake.traced "Compile template"
      $ Mus.localAutomaticCompile templatePath
  case eTemplate of
    Right template -> do
      Shake.need . Mus.getPartials . Mus.ast $ template
      --Shake.putInfo $ "Read " <> templatePath
      return template
    Left err -> fail $ show err

fromPost :: Post -> RenderedPost
fromPost post =
  RenderedPost
    { rPostTitle = postTitle post,
      rPostAuthor = postAuthor post,
      rPostTags = postTags post,
      rPostHasTags = not . null . postTags $ post,
      rPostDate = postDate post,
      rPostIsoDate = postDate post >>= parseDate,
      rPostContent = postContent post,
      rPostLink = postLink post,
      -- maybe can replace non-acceptable chars with - but unclear is exactly one - is allowed or now https://www.iana.org/assignments/urn-informal/urn-1
      rPostId = T.pack . ("urn:urn-1:" ++) . filter (\c -> elem c "abcdefghijklmnopqrstuvwxyz0123456789") . T.unpack . T.toLower . postTitle $ post,
      rPostSummary = postDescription post
    }
