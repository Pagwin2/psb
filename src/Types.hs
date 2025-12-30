module Types where

import Data.Aeson (ToJSON (toJSON))
import Data.Text (Text)
import Deriving.Aeson
import Deriving.Aeson.Stock (PrefixedSnake)
import Text.Mustache (ToMustache (toMustache))

-- pageSection is what css class should be specified in a style html element, I would do an enum but I foresee that being a mistake
data Page = Page
  { pageTitle :: Text,
    pageContent :: Text,
    -- build time
    pageNow :: Text,
    --
    pageUrl :: Text
  }
  deriving (Show, Generic)
  deriving (ToJSON) via PrefixedSnake "page" Page

instance ToMustache Page where
  toMustache = toMustache . toJSON

data RenderedPost = RenderedPost
  { rPostTitle :: Text,
    rPostAuthor :: Maybe Text,
    rPostTags :: [Text],
    rPostHasTags :: Bool,
    rPostDate :: Maybe Text,
    rPostIsoDate :: Maybe Text,
    rPostContent :: Maybe Text,
    rPostLink :: Maybe Text,
    rPostSummary :: Maybe Text,
    rPostId :: Text
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "rPost" RenderedPost

instance ToMustache RenderedPost where
  toMustache = toMustache . toJSON

data Post = Post
  { postTitle :: Text,
    postAuthor :: Maybe Text,
    postTags :: [Text],
    postDate :: Maybe Text,
    postContent :: Maybe Text,
    postLink :: Maybe Text,
    postDescription :: Maybe Text,
    postDraft :: Maybe Bool
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "post" Post

instance ToMustache Post where
  toMustache = toMustache . toJSON
