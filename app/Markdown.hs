{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Markdown (markdownParser) where

import Data.Functor
import Data.Text
import IR
import Text.Parsec
import Text.Parsec.Combinator

type Parser a = forall s u m. (Stream s m Char) => ParsecT s u m a

markdownParser :: Parser Document
markdownParser = Doc <$> many block

block :: Parser Element
block = choice [heading, codeBlock, quoteBlock, list, table, htmlBlock, paragraph, blankLine]

heading :: Parser Element
heading = pure $ Heading $ H {level = 1, text = ""}

codeBlock :: Parser Element
codeBlock = pure $ Code $ C {language = "", code = ""}

quoteBlock :: Parser Element
quoteBlock = pure $ BlockQuote $ Q ""

list :: Parser Element
list = pure $ List $ L {list_type = Ordered, items = []}

table :: Parser Element
table = pure $ Table $ T {header = TH [], rows = []}

htmlBlock :: Parser Element
htmlBlock = pure $ HTML $ Raw ""

paragraph :: Parser Element
paragraph = do
  first <- paragraphLine
  rem <- many paragraphContinuation
  let combined = Prelude.concat (first : rem)
  pure $ Paragraph $ P combined

paragraphLine :: Parser [InlineText]
paragraphLine = many inlineText <* endOfLine

paragraphContinuation :: Parser [InlineText]
paragraphContinuation = notFollowedBy blockElemStart *> paragraphLine

inlineText :: Parser InlineText
inlineText = choice [emphasis, strong, inlineCode, link, image, inlineHTML, escapedChar, plainText]

plainText :: Parser InlineText
-- abnf is very specific about what's allowed due to actual ABNF not allowing negation but I'm lazy
plainText = fmap (Normal . pack) $ many $ noneOf "*_`[]()<>#+-.!&\\\n"

escapedChar :: Parser InlineText
escapedChar = char '\\' *> fmap Escaped visibleChar

htmlInline :: Parser InlineText
htmlInline = do
  char '<'
  tagName <- name
  remaining <- htmlInlineRemainder
  whiteSpace
  char '>'
  let remainingTagText = foldl' (\ongoing current -> ongoing ++ ' ' : current) "" remaining

  pure $ HTMLIn $ pack $ '<' : name ++ remaining
  where
    htmlInlineRemainder = many $ whiteSpace *> attribute
    name = many $ choice [alphaNum, char '-', char ':']
    value = do
      char '"'
      l <- letter
      rem <- many $ choice [alphaNum, char '-', char ':']
      char '"'
      pure '"' : l : rem ++ "\""
    attribute = do
      attrName <- name
      char '='
      attrValue <- value
      pure attrName ++ ('=' : attrValue)

whiteSpace :: Parser Text
whiteSpace = pack <$> many space

visibleChar :: Parser Char
-- technically more strict but I'm just going to hope I never have to deal with that
visibleChar = anyChar
