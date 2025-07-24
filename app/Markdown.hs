{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Markdown (markdownParser) where

import Data.Text
import IR
import Text.Parsec
import Text.Parsec.Combinator

type Parser a = forall s u m t. (Stream s m t) => ParsecT s u m a

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
paragraph = pure $ Paragraph $ P ""

blankline :: Parser Element
blankline = pure $ BlankLine BL
