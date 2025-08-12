{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Markdown (markdownParser) where

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
  pure $ Paragraph $ P []

paragraphLine :: Parser [InlineText]
paragraphLine = many inlineText <* endOfLine

paragraphContinuation :: Parser [InlineText]
paragraphContinuation = notFollowedBy blockElemStart *> paragraphLine

inlineText :: Parser InlineText
inlineText = choice [emphasis, strong, inlineCode, link, image, inlineHTML, paragraphLineBreak, escapedChar, plainText]

plainText :: Parser Text
plainText = 

blankLine :: Parser Element
blankLine = do
  endOfLine
  pure $ BlankLine BL
