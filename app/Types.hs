module Types where

import Data.Text (Text)
import Deriving.Aeson
import Deriving.Aeson.Stock (PrefixedSnake)

data Page = Page {pageTitle :: Text, pageContent :: Text}
  deriving (Show, Generic)
  deriving (ToJSON) via PrefixedSnake "page" Page

data RenderedPost = RenderedPost
  { rPostTitle :: Text,
    rPostAuthor :: Maybe Text,
    rPostTags :: [Text],
    rPostHasTags :: Bool,
    rPostDate :: Maybe Text,
    rPostContent :: Maybe Text,
    rPostLink :: Maybe Text
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "rPost" RenderedPost

data Post = Post
  { postTitle :: Text,
    postAuthor :: Maybe Text,
    postTags :: [Text],
    postDate :: Maybe Text,
    postContent :: Maybe Text,
    postLink :: Maybe Text,
    postDraft :: Bool
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "post" Post
