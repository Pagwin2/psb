module IR where

import Data.Text

newtype Document = Doc [Element]

data Element
  = Heading Heading
  | Code Code
  | BlockQuote BlockQuote
  | List List
  | HTML HTML
  | Paragraph Paragraph
  | HorizontalRule

-- Removed: BlankLine

data Heading = H
  { level :: Int,
    text :: [InlineText]
  }

data Code = C
  { language :: Maybe Text,
    code :: Text
  }

data BlockQuote = Q [InlineText]

data ListItem = LI
  { content :: [InlineText], -- Flatten continuations into here
    children :: [List]
  }

data ListType = Ordered | Unordered

data List = L
  { list_type :: ListType,
    items :: [ListItem]
  }

-- Table: keep as-is or simplify based on your needs

data HTML
  = HTMLTag
  { tagName :: Text,
    attributes :: [(Text, Maybe Text)],
    html_content :: Text
  }

-- Optionally skip: HTMLComment, HTMLDeclaration

newtype Paragraph = P [InlineText]

data InlineText
  = Text Text -- Combined Normal and Escaped
  | Bold [InlineText]
  | Italic [InlineText]
  | InlineCode Text
  | Link
      { linkText :: [InlineText],
        url :: Text,
        title :: Maybe Text
      }
  | Image
      { altText :: [InlineText],
        url :: Text,
        title :: Maybe Text
      }
  | HTMLInline
      { inlineTagName :: Text,
        inlineAttributes :: [(Text, Maybe Text)]
      }

-- for processing math
-- https://hackage.haskell.org/package/typst-0.6.1/docs/Typst-Parse.html#v:parseTypst
-- and
-- https://hackage.haskell.org/package/typst-symbols-0.1.7/docs/Typst-Symbols.html
-- are going to be used for handling typst and
-- texmath for latex handling
