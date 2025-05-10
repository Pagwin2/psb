{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Markdown.Data
  ( MarkDownElement,
    MListItem,
    Parser,
  )
where

import Data.Text
import Text.Parsec

--  modified version of https://github.com/tusharad/markdown-parser
data MarkDownElement
  = MHeading Int MarkDownElement
  | MParagraph MarkDownElement
  | MBold MarkDownElement
  | MItalic MarkDownElement
  | MBoldItalic MarkDownElement
  | MLink MarkDownElement Text
  | MLine [MarkDownElement]
  | MUnorderedList [MListItem]
  | MOrderedList [MListItem]
  | Only Text -- Bottom of all types
  deriving (Eq, Show)

data MListItem = MListItem MarkDownElement [MarkDownElement]
  deriving (Eq, Show)

type Parser v = forall s u m. (Monad m, Stream s m Char) => ParsecT s u m v
