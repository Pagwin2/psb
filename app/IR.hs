module IR where

import Data.Text

newtype Document = Doc [Element]
  deriving (Show)

data Element
  = Heading Heading
  | Code Code
  | BlockQuote BlockQuote
  | List List
  | HTML HTML
  | Paragraph Paragraph
  | HorizontalRule
  deriving (Show)

-- Removed: BlankLine

data Heading = H
  { level :: Int,
    text :: [InlineText]
  }
  deriving (Show)

data Code = C
  { language :: Maybe Text,
    code :: Text
  }
  deriving (Show)

data BlockQuote = Q [InlineText] deriving (Show)

data ListItem = LI
  { content :: [InlineText], -- Flatten continuations into here
    children :: [List]
  }
  deriving (Show)

data ListType = Ordered | Unordered deriving (Show)

data List = L
  { list_type :: ListType,
    items :: [ListItem]
  }
  deriving (Show)

data HTML
  = HTMLTag
  { html_content :: Text
  }
  deriving (Show)

newtype Paragraph = P [InlineText] deriving (Show)

data InlineText
  = Text Text -- Combined Normal and Escaped
  | Bold [InlineText]
  | Italic [InlineText]
  | Crossed [InlineText]
  | InlineCode Text
  | Link
      { linkText :: [InlineText],
        url :: Text,
        title :: Maybe Text
      }
  | Image
      { altText :: Text,
        url :: Text,
        title :: Maybe Text
      }
  | HTMLInline {inline_html_content :: Text}
  deriving (Show)

-- for processing math
-- https://hackage.haskell.org/package/typst-0.6.1/docs/Typst-Parse.html#v:parseTypst
-- and
-- https://hackage.haskell.org/package/typst-symbols-0.1.7/docs/Typst-Symbols.html
-- are going to be used for handling typst and
-- texmath for latex handling
