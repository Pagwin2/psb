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
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, newline, spaceChar, string)

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
inlineText :: (Logger m, Characters s) => Parser s m InlineText
inlineText = inlineText' (fail "notFollowedBy noop")
  where
    inlineText' disallow = choice [try $ strikethrough disallow, try $ bold disallow, try $ italic disallow, try $ underlined disallow, try code, try $ link disallow, try $ image disallow, plain_text disallow]
    between' start end middle_piece = between start end $ many ((notFollowedBy end) *> middle_piece)

    strikethrough disallow = Crossed <$> (between' (string "~~") (string "~~") (inlineText' (disallow <|> (void $ string "~~"))))

    bold disallow = Bold <$> (between' (string "**") (string "**") (inlineText' (disallow <|> (void $ string "**"))))

    italic disallow = Italic <$> (between' (char '*') (char '*') (inlineText' (disallow <|> (void $ char '*'))))

    underlined disallow = Underlined <$> (between' (string "__") (string "__") (inlineText' (disallow <|> (void $ string "__"))))

    code = InlineCode . T.pack <$> (between' (char '`') (char '`') (notFollowedBy lineEnding *> anySingle))

    link disallow = do
      linkText <- between' (char '[') (char ']') (inlineText' (disallow <|> (void $ char ']')))
      (url, title) <- do
        char '('
        -- might fail on newline char situation
        url <- T.pack <$> (many (notFollowedBy (char ')' <|> spaceChar) *> anySingle))
        hasTitle <- optional spaceChar
        title <- case hasTitle of
          Just _ -> Just . T.pack <$> (many (notFollowedBy ((void $ char ')') <|> lineEnding) *> anySingle))
          Nothing -> pure Nothing
        char ')'
        pure (url, title)
      pure Link {linkText, url, title}

    image disallow = do
      char '!'
      -- Is this a hack? Yes. Bite me
      link_hack <- link disallow
      (altText, url, title) <- case link_hack of
        Link {linkText = [Text altText], url, title} -> pure (altText, url, title)
        _ -> fail "Image alt text must be normal text, cannot be stylized in any way"
      pure Image {altText, url, title}

    plain_text disallow = Text . T.pack <$> (many ((notFollowedBy (blockEnding <|> disallow)) *> anySingle))

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

-- type of list the parser returns
-- parser which grabs the prefix for each item of the list
-- parser used for child lists
-- nesting amount
listBlock :: (Logger m, Characters s) => ListType -> Parser s m prefix -> (Int -> Parser s m List) -> Int -> Parser s m Element
listBlock list_type prefix child_parser_factory nest_level = do
  error "unhandled ident_level"
  items <- some $ (try (listItem <* notFollowedBy blockEnding)) <|> (listItem <* lineEnding)
  pure $ List $ L {list_type, items}
  where
    listItem = do
      prefix
      content <- many inlineText
      child <- optional $ child_parser_factory $ nest_level + 1
      pure $ LI {content, child}

unorderedListBlock :: (Logger m, Characters s) => Int -> Parser s m Element
unorderedListBlock = listBlock Unordered unordered_prefix (\level -> unwrap <$> ((try $ unorderedListBlock level) <|> orderedListBlock level))
  where
    unordered_prefix = (choice $ map char "*-+") *> optional spaceChar
    -- not exhaustive but we know listBlock is returning a List
    unwrap (List l) = l

orderedListBlock :: (Logger m, Characters s) => Int -> Parser s m Element
orderedListBlock = listBlock Ordered ordered_prefix (\level -> unwrap <$> ((try $ unorderedListBlock level) <|> orderedListBlock level))
  where
    ordered_prefix = error "ordered_prefix"
    -- not exhaustive but we know listBlock is returning a List
    unwrap (List l) = l

htmlBlock :: (Logger m, Characters s) => Parser s m Element
htmlBlock = error "TODO: htmlBlock"

paragraphBlock :: (Logger m, Characters s) => Parser s m Element
paragraphBlock = Paragraph . P <$> (many inlineText)
