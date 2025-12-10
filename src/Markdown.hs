{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- (document, metadata)
module Markdown (document, metadata) where

import Control.Applicative (many, optional, some, (<|>))
import Control.Monad (guard, void)
import Data.Functor.Identity (Identity)
import Data.Maybe (maybeToList)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import IR
import Logger (Logger (logDebug))
import Text.Megaparsec (ParsecT, Stream, Token, Tokens, anySingle, anySingleBut, between, choice, count, eof, manyTill, notFollowedBy, satisfy, skipSome, try, (<?>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, newline, string)

type Parser = ParsecT Void

class (Token s ~ Char, Stream s, IsString (Tokens s), Show s) => Characters s

instance Characters Text

instance Characters String

metadata :: (Logger m, Characters s) => Parser s m Text
metadata = bound *> fmap T.pack (many $ try (char '-' <* notFollowedBy (string "--")) <|> anySingleBut '-') <* bound
  where
    bound = string "---"

document :: (Logger m, Characters s) => Parser s m Document
document = Doc <$> many element

element :: (Logger m, Characters s) => Parser s m Element
element =
  choice
    [ try headingBlock <?> "Element Heading",
      try fencedCodeBlock <?> "Fenced Code Block",
      try blockquoteBlock <?> "BlockQuote",
      (try $ unorderedListBlock 0) <?> "Unordered List",
      (try $ orderedListBlock 0) <?> "Ordered List",
      try htmlBlock <?> "HTML Block",
      paragraphBlock <?> "Paragarph"
    ]
    <* blockEnding

lineEnding :: (Logger m, Characters s) => Parser s m ()
lineEnding = (try eof) <|> void newline

-- we don't need to parse eof, lineEnding does that, eof *> eof works just fine in place of eof
blockEnding :: (Logger m, Characters s) => Parser s m ()
blockEnding = lineEnding *> lineEnding

-- TODO: check if inlineHTML needs to be handled in any markdown posts
-- TODO: link impl
-- TODO: image impl
inlineText :: (Logger m, Characters s) => Parser s m InlineText
inlineText = choice [try strikethrough, try bold, try italic, try underlined, try code, try link, try image, plain_text]
  where
    between' start end middle_piece = between start end $ many ((notFollowedBy end) *> middle_piece)

    strikethrough = Crossed <$> (between' (string "~~") (string "~~") inlineText)

    bold = Bold <$> (between' (string "**") (string "**") inlineText)

    italic = Italic <$> (between' (char '*') (char '*') inlineText)

    underlined = Underlined <$> (between' (string "__") (string "__") inlineText)

    code = InlineCode . T.pack <$> (between' (char '`') (char '`') (notFollowedBy lineEnding *> anySingle))

    link = do
      linkText <- error "linkText parser"
      url <- error "url parser"
      title <- error "title parser"
      pure Link {linkText, url, title}

    image = do
      altText <- error "altText"
      url <- error "url"
      title <- error "title"
      pure Image {altText, url, title}

    plain_text = Text . T.pack <$> (many ((notFollowedBy blockEnding) *> anySingle))

headingBlock :: (Logger m, Characters s) => Parser s m Element
headingBlock = do
  heading_level <- length <$> (some $ char '#')
  optional $ char ' '
  text <- many $ inlineText
  pure $ Heading $ H {level = heading_level, text}

fencedCodeBlock :: (Logger m, Characters s) => Parser s m Element
fencedCodeBlock = between (string "```") (string "```") $ do
  language' <- T.pack <$> (many (notFollowedBy lineEnding *> anySingle))
  lineEnding
  code <- T.pack <$> (many ((notFollowedBy $ string "```") *> anySingle))
  let language = if language' == "" then Just language' else Nothing
  pure $ Code $ C {language, code}

blockquoteBlock :: (Logger m, Characters s) => Parser s m Element
blockquoteBlock = BlockQuote . Q . concat <$> (some blockquoteLine)
  where
    blockquoteLine = do
      char '>'
      optional $ char ' '
      ret <- (many ((notFollowedBy lineEnding) *> inlineText))
      -- this dance with optional and notFollowedBy is done so we
      -- aren't accidentally consuming part of a block ending
      (optional ((notFollowedBy blockEnding) *> lineEnding))
      pure ret

unorderedListBlock :: (Logger m, Characters s) => Int -> Parser s m Element
unorderedListBlock indent_level = do
  error "Can unordered and ordered lists be consolidated into 1 function of ListType -> Parser s m prefix -> Int -> Paser s m Element"
  error "unhandled ident_level"
  items <- some $ (try (unorderedListItem <* notFollowedBy blockEnding)) <|> (unorderedListItem <* lineEnding)
  pure $ List $ L {list_type = Unordered, items}
  where
    unorderedListItem = error "unorderedListItem"

orderedListBlock :: (Logger m, Characters s) => Int -> Parser s m Element
orderedListBlock indent_level = do
  error "unhandled ident_level"
  items <- some $ (try (orderedListItem <* notFollowedBy blockEnding)) <|> (orderedListItem <* lineEnding)
  pure $ List $ L {list_type = Unordered, items}
  where
    orderedListItem = error "orderedListItem"

htmlBlock :: (Logger m, Characters s) => Parser s m Element
htmlBlock = error "TODO: htmlBlock"

paragraphBlock :: (Logger m, Characters s) => Parser s m Element
paragraphBlock = Paragraph . P <$> (many inlineText)
