{-# LANGUAGE FlexibleContexts #-}

module Markdown.Parser
  ( Markdown.Parser.lines,
  )
where

import Data.Functor (void)
import Data.Text (Text, pack)
import Text.Parsec

-- https://spec.commonmark.org/0.31.2/
-- https://hackage.haskell.org/package/parsec

linebreak ::
  (Monad m, Stream s m Char) =>
  ParsecT s u m ()
-- 2 newlines due to mark
linebreak = void (newline *> newline)

emptyParse ::
  (Monad m, Stream s m Char) =>
  ParsecT s u m String
emptyParse = "" <$ notFollowedBy anyChar

line ::
  (Monad m, Stream s m Char) =>
  ParsecT s u m Text
line = fmap pack $ many $ notFollowedBy linebreak *> anyChar

lines ::
  (Monad m, Stream s m Char) =>
  ParsecT s u m [Text]
lines = line `sepBy` linebreak

heading ::
  (Monad m, Stream s m Char) =>
  ParsecT s u m (Int, Text)
heading = do
  level <- fmap length $ many1 $ char '#'
  text <- line
  pure (level, text)

-- TODO: blockquote, single backticks, triple backticks, links, arb HTML
