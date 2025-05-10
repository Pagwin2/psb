{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Markdown.Parser
  ( heading,
  )
where

import Markdown.Data
import Text.Parsec

-- https://spec.commonmark.org/0.31.2/
-- https://hackage.haskell.org/package/parsec

-- only ATX headings
heading :: Parser MarkdownElement
heading = do
  -- technically this can lead to illegal heading levels but
  -- all the input is written by me so who cares
  level <- fmap length $ try $ many1 $ char '#'

  return $ MHeading level $ Only "TODO"

rawHTML
