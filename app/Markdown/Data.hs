{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Markdown.Data
  ( MListItem,
    Parser,
    MarkdownElement
      ( MHeading,
        MParagraph,
        MBold,
        MItalic,
        MBoldItalic,
        MLink,
        MLine,
        MUnorderedList,
        MOrderedList,
        Only
      ),
  )
where

import Data.Text
import Text.Parsec

-- very slightly modified version of https://github.com/tusharad/markdown-parser
data MarkdownElement
  = MHeading Int MarkdownElement
  | MParagraph MarkdownElement
  | MBold MarkdownElement
  | MItalic MarkdownElement
  | MBoldItalic MarkdownElement
  | MLink MarkdownElement Text
  | MLine [MarkdownElement]
  | MUnorderedList [MListItem]
  | MOrderedList [MListItem]
  | Raw Text -- HTML contained in the markdown
  | Only Text -- Bottom of all types
  deriving (Eq, Show)

data MListItem = MListItem MarkdownElement [MarkdownElement]
  deriving (Eq, Show)

type Parser v = forall s u m. (Monad m, Stream s m Char) => ParsecT s u m v
