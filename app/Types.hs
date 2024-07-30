{-# LANGUAGE ApplicativeDo, DataKinds, DeriveGeneric #-}
{-# LANGUAGE DerivingVia, TypeApplications #-}

module Types where

import Deriving.Aeson
import Deriving.Aeson.Stock (PrefixedSnake)
import Data.Text (Text)

data Page = Page {pageTitle :: Text, pageContent :: Text}
  deriving (Show, Generic)
  deriving (ToJSON) via PrefixedSnake "page" Page

data Post = Post
  { postTitle :: Text,
    postAuthor :: Maybe Text,
    postTags :: [Text],
    postDate :: Maybe Text,
    postContent :: Maybe Text,
    postLink :: Maybe Text
  } deriving (Show, Generic)
    deriving (FromJSON, ToJSON) via PrefixedSnake "post" Post
