{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- (document, metadata)
module Markdown (document, metadata) where

import Control.Applicative (many, optional, some, (<|>))
import Control.Monad (guard, void)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import IR
import Logger (Logger (logCallStack, logDebug))
import Text.Megaparsec (ParsecT, Stream, Token, Tokens, anySingle, anySingleBut, between, choice, chunk, count, eof, lookAhead, manyTill, notFollowedBy, satisfy, sepBy, skipSome, try, (<?>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, newline, space, spaceChar)
import qualified Text.Megaparsec.Stream as MPS

type Parser = ParsecT Void

class (Token s ~ Char, Stream s, ToText (Tokens s), IsString (Tokens s), Monoid (Tokens s), Eq (Tokens s), Show s) => Characters s

class ToText t where
  toText :: t -> Text

instance Characters Text

instance ToText Text where
  toText = id

instance Characters String

instance ToText String where
  toText = T.pack

string :: (MP.MonadParsec e s m) => Tokens s -> m (Tokens s)
string = chunk

metadata :: (Logger m, Characters s) => Parser s m Text
metadata = bound *> fmap T.pack (many $ try (char '-' <* notFollowedBy (chunk "--")) <|> anySingleBut '-') <* bound
  where
    bound = string "---"

document :: (Logger m, Characters s) => Parser s m Document
document = Doc <$> many ((notFollowedBy eof) *> element)

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
    <* logDebug "element end"
    <* blockEnding

lineEnding :: (Logger m, Characters s, HasCallStack) => Parser s m ()
lineEnding = {-logCallStack *>-} ((try eof) <|> void newline)

-- we don't need to parse eof, lineEnding does that, eof *> eof works just fine in place of eof
blockEnding :: (Logger m, Characters s, HasCallStack) => Parser s m ()
blockEnding = lineEnding *> lineEnding

inlineText :: forall m s. (HasCallStack, Logger m, Characters s) => Parser s m InlineText
inlineText = inlineText' $ fail "noop on notFollowedBy"
  where
    inlineText' :: (HasCallStack) => Parser s m () -> Parser s m InlineText
    inlineText' disallow = choice [try $ strikethrough disallow, try $ bold disallow, try $ italic disallow, try $ underlined disallow, try code, try $ link disallow, try $ image disallow, try inline_html, plain_text disallow]

    between' start end middle_piece = between start end $ many ((notFollowedBy ((try $ void end) <|> blockEnding)) *> middle_piece)

    strikethrough disallow = Crossed <$> (between' (string "~~") (disallow <|> (void $ string "~~")) (inlineText' (disallow <|> (void $ string "~~"))))

    -- TODO: bold and italic eat a lineEnding that they shouldn't for some reason
    bold disallow = Bold <$> (between' (string "**") (disallow <|> (void $ string "**")) (inlineText' (disallow <|> (void $ string "**"))))

    italic :: (HasCallStack) => Parser s m () -> Parser s m InlineText
    italic disallow = Italic <$> (between' (char '*') ((void $ char '*') <|> disallow) (inlineText' (disallow <|> (void $ char '*'))))

    underlined disallow = Underlined <$> (between' (string "__") ((void $ string "__") <|> disallow) (inlineText' (disallow <|> (void $ string "__"))))

    code = InlineCode . T.pack <$> (between' (char '`') (char '`') ((notFollowedBy lineEnding) *> anySingle))
    link :: (HasCallStack) => Parser s m () -> Parser s m InlineText
    link disallow = do
      linkText <- between' (char '[') ((void $ char ']') <|> disallow) (inlineText' (disallow <|> (void $ char ']')))
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

    inline_html =
      HTMLInline <$> do
        char '<'
        inner <- toText . MPS.tokensToChunk (Proxy :: Proxy s) <$> (many (anySingleBut '>'))
        char '>'
        pure $ mconcat ["<", inner, ">"]

    plain_text disallow = Text . T.pack <$> (many ((notFollowedBy (blockEnding <|> disallow)) *> anySingle))

headingBlock :: (Logger m, Characters s) => Parser s m Element
headingBlock = do
  heading_level <- length <$> (some $ char '#')
  optional spaceChar
  text <- many ((notFollowedBy blockEnding) *> inlineText)
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
  items <- some $ (try (listItem <* notFollowedBy blockEnding)) <|> (listItem <* lineEnding)
  pure $ List $ L {list_type, items}
  where
    listItem = do
      count nest_level ((try $ void $ char '\t') <|> (void $ string "    "))
      prefix
      content <- many ((notFollowedBy lineEnding) *> inlineText)
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
    -- regex equivalent: [0-9]+[.)]\s?
    ordered_prefix = (some digitChar) *> (char '.' <|> char ')') *> optional spaceChar
    -- not exhaustive but we know listBlock is returning a List
    unwrap (List l) = l

htmlBlock :: forall m s. (Logger m, Characters s) => Parser s m Element
htmlBlock = do
  char '<'
  tagName <- MPS.tokensToChunk (Proxy :: Proxy s) <$> (some ((notFollowedBy ((try $ void tagNameEnd) <|> blockEnding)) *> (anySingle :: Parser s m (Token s))))
  notFollowedBy blockEnding
  ending <- tagNameEnd
  hasEnded <- case ending of
    '>' -> pure True
    _ -> pure False
  attrs <-
    if not hasEnded
      then
        Just . toText . mconcat <$> htmlAttrs
      else pure Nothing
  -- technically not standard markdown but I don't want to write a full HTML parser in my
  inside <- many (notFollowedBy ((chunk $ "</" <> tagName <> ">") <|> chunk "</>") *> anySingle)
  end <- toText <$> ((chunk $ "</" <> tagName <> ">") <|> chunk "</>")
  pure $ HTML $ HTMLTag $ T.concat ["<", toText tagName, fromMaybe "" attrs, ">", T.pack inside, if end == "</>" then "" else end]
  where
    tagNameEnd = (lookAhead spaceChar <* space) <|> char '>'
    htmlAttrs = ((notFollowedBy $ char '>') *> htmlAttr) `sepBy` space
    htmlAttr = do
      name <- many (notFollowedBy (lineEnding <|> (void $ char '=')) *> anySingle)
      char '='
      char '"'
      value <- many (notFollowedBy (lineEnding <|> (void $ char '"')) *> anySingle)
      char '"'
      pure $ mconcat [name, "=\"", value, "\""]

paragraphBlock :: (Logger m, Characters s) => Parser s m Element
paragraphBlock = Paragraph . P <$> (many ((notFollowedBy blockEnding) *> inlineText))
