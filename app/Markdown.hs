{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- (document, metadata)
module Markdown where

import Control.Applicative (many, optional, some, (<|>))
import Control.Monad (guard, void)
import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Text as T
import IR
import Text.Parsec hiding (Line, many, optional, (<|>))
import Text.Parsec.String (Parser)

metadata :: Parser Text
metadata = bound *> fmap T.pack (many $ try (char '-' <* notFollowedBy (string "--")) <|> noneOf "-") <* bound
  where
    bound = string "---"

document :: Parser Document
document = Doc <$> many element <* eof

element :: Parser Element
element =
  choice
    [ try headingBlock,
      try fencedCodeBlock,
      try indentedCodeBlock,
      try blockquoteBlock,
      try unorderedListBlock,
      try orderedListBlock,
      try horizontalRuleBlock,
      try htmlBlock,
      try blankLines, -- Consume blank lines but don't add to AST
      paragraphBlock
    ]

-- Blank lines (consumed but not stored)
blankLines :: Parser Element
blankLines = do
  skipMany1 blankLine
  element <|> fmap (const $ HTML HTMLTag {html_content = ""}) eof -- Parse the next element (or handle eof)

blankLine :: Parser ()
blankLine = do
  many (char ' ' <|> char '\t')
  lineEnding
  pure ()

-- Heading Block
headingBlock :: Parser Element
headingBlock = do
  hashes <- some (char '#')
  let level = length hashes
  guard (level <= 6)
  many (char ' ' <|> char '\t')
  content <- manyTill inlineElement (try lineEnding)
  pure $ Heading $ H level content

-- Fenced Code Block
fencedCodeBlock :: Parser Element
fencedCodeBlock = do
  fence <- string "```" <|> string "~~~"
  lang <- optionMaybe languageInfo
  lineEnding
  codeLines <- manyTill codeLine (try $ string fence)
  pure $ Code $ C lang (T.pack $ unlines codeLines)

languageInfo :: Parser Text
languageInfo = T.pack <$> many1 (alphaNum <|> char '-' <|> char '+' <|> char '.')

codeLine :: Parser String
codeLine = do
  line <- many (noneOf "\n\r")
  lineEnding
  pure line

-- Indented Code Block
indentedCodeBlock :: Parser Element
indentedCodeBlock = do
  lines' <- some indentedLine
  pure $ Code $ C Nothing (T.pack $ unlines lines')
  where
    indentedLine = do
      count 4 (char ' ' <|> char '\t')
      line <- many (noneOf "\n\r")
      lineEnding
      pure line

-- Blockquote Block
blockquoteBlock :: Parser Element
blockquoteBlock = do
  lines' <- some blockquoteLine
  pure $ BlockQuote $ Q (concat lines')
  where
    blockquoteLine = do
      char '>'
      optional (char ' ')
      content <- manyTill inlineElement (try lineEnding)
      pure content

-- Horizontal Rule Block
horizontalRuleBlock :: Parser Element
horizontalRuleBlock = do
  choice
    [ try (count 3 (char '*') >> many (char ' ' <|> char '*')),
      try (count 3 (char '-') >> many (char ' ' <|> char '-')),
      try (count 3 (char '_') >> many (char ' ' <|> char '_'))
    ]
  lineEnding
  pure HorizontalRule

-- Unordered List Block
unorderedListBlock :: Parser Element
unorderedListBlock = do
  items <- some unorderedListItem
  pure $ List $ L Unordered items

unorderedListItem :: Parser ListItem
unorderedListItem = do
  oneOf "*-+"
  char ' ' <|> char '\t'
  content <- manyTill inlineElement (try lineEnding)
  -- continuations <- many listContinuation
  children <- many (try indentedList)
  pure $ LI content children

listContinuation :: Parser [InlineText]
listContinuation = do
  count 2 (char ' ' <|> char '\t')
  many (char ' ' <|> char '\t')
  notFollowedBy (oneOf "*-+")
  notFollowedBy (digit >> char '.')
  content <- manyTill inlineElement (try lineEnding)
  pure content

-- TODO: handle list indentation at all levels
indentedList :: Parser List
indentedList = do
  let n = 1
  void $ count (4 * n) (char ' ') <|> count n (char '\t')
  choice [try indentedUnorderedList, indentedOrderedList]

indentedUnorderedList :: Parser List
indentedUnorderedList = do
  items <- some (try $ indentedListItem (oneOf "*-+" >> void (char ' ' <|> char '\t')))
  pure $ L Unordered items

indentedOrderedList :: Parser List
indentedOrderedList = do
  items <- some (try $ indentedListItem (some digit >> char '.' >> void (char ' ' <|> char '\t')))
  pure $ L Ordered items

indentedListItem :: Parser () -> Parser ListItem
indentedListItem marker = do
  marker
  content <- manyTill inlineElement (try $ lineEnding <|> eof)
  pure $ LI content []

-- Ordered List Block
orderedListBlock :: Parser Element
orderedListBlock = do
  items <- some orderedListItem
  pure $ List $ L Ordered items

orderedListItem :: Parser ListItem
orderedListItem = do
  some digit
  char '.'
  char ' ' <|> char '\t'
  content <- manyTill inlineElement (try lineEnding)
  continuations <- many listContinuation
  children <- many (try indentedList)
  pure $ LI (content ++ concat continuations) children

-- HTML Block
htmlBlock :: Parser Element
htmlBlock = do
  char '<'
  -- Capture the entire HTML block as raw text
  rest <- manyTill anyChar (try $ char '>' >> lineEnding)
  let content = '<' : rest
  return $ HTML $ HTMLTag (T.pack content)

tagName :: Parser String
tagName = do
  first <- satisfy isAlpha
  rest <- many (alphaNum <|> char '-' <|> char ':')
  pure (first : rest)

attribute :: Parser (Text, Maybe Text)
attribute = do
  name <- attributeName
  value <- optionMaybe (char '=' >> attributeValue)
  pure (T.pack name, fmap T.pack value)

attributeName :: Parser String
attributeName = do
  first <- satisfy isAlpha
  rest <- many (alphaNum <|> char '-' <|> char ':')
  pure (first : rest)

attributeValue :: Parser String
attributeValue =
  choice
    [ between (char '"') (char '"') (many $ noneOf "\""),
      between (char '\'') (char '\'') (many $ noneOf "'"),
      some (noneOf " \t\n\r>\"'=<`")
    ]

-- Paragraph Block
paragraphBlock :: Parser Element
paragraphBlock = do
  content <- some inlineElement
  lineEnding <|> eof
  pure $ Paragraph $ P content

-- Inline Elements
inlineElement :: Parser InlineText
inlineElement =
  choice
    [ try strong,
      try emphasis,
      try crossedText,
      try codeSpan,
      try image,
      try link,
      try htmlInline,
      try escapedChar,
      plainText
    ]

-- Strong (Bold)
strong :: Parser InlineText
strong = strongAsterisk <|> strongUnderscore

strongAsterisk :: Parser InlineText
strongAsterisk = do
  string "**"
  content <- some (notFollowedBy (string "**") >> inlineElement)
  string "**"
  pure $ Bold content

strongUnderscore :: Parser InlineText
strongUnderscore = do
  string "__"
  content <- some (notFollowedBy (string "__") >> inlineElement)
  string "__"
  pure $ Bold content

crossedText :: Parser InlineText
crossedText = do
  string "~~"
  content <- some (notFollowedBy (string "~~") >> inlineElementNo '~')
  string "~~"
  pure $ Crossed content

-- Emphasis (Italic)
emphasis :: Parser InlineText
emphasis = emphasisAsterisk <|> emphasisUnderscore

emphasisAsterisk :: Parser InlineText
emphasisAsterisk = do
  char '*'
  content <- some (notFollowedBy (char '*') >> inlineElementNoAsterisk)
  char '*'
  pure $ Italic content

emphasisUnderscore :: Parser InlineText
emphasisUnderscore = do
  char '_'
  content <- some inlineElementNoUnderscore
  char '_'
  pure $ Italic content

inlineElementNo :: Char -> Parser InlineText
inlineElementNo c =
  choice
    [ try strong,
      try codeSpan,
      try image,
      try link,
      try htmlInline,
      try escapedChar,
      plainTextNo c
    ]

plainTextNo :: Char -> Parser InlineText
plainTextNo c = fmap (Text . T.pack) $ some $ noneOf [c, '\n']

inlineElementNoAsterisk :: Parser InlineText
inlineElementNoAsterisk =
  choice
    [ try strong,
      try codeSpan,
      try image,
      try link,
      try htmlInline,
      try escapedChar,
      plainTextNo '*'
    ]

inlineElementNoUnderscore :: Parser InlineText
inlineElementNoUnderscore =
  choice
    [ try strong,
      try codeSpan,
      try image,
      try link,
      try htmlInline,
      try escapedChar,
      plainTextNo '_'
    ]

-- Code Span
codeSpan :: Parser InlineText
codeSpan =
  choice
    [ try tripleBacktick,
      try doubleBacktick,
      singleBacktick
    ]
  where
    singleBacktick = do
      char '`'
      content <- many (noneOf "`\n\r")
      char '`'
      pure $ InlineCode (T.pack content)
    doubleBacktick = do
      string "``"
      content <- manyTill anyChar (try $ string "``")
      pure $ InlineCode (T.pack content)
    tripleBacktick = do
      string "```"
      content <- manyTill anyChar (try $ string "```")
      pure $ InlineCode (T.pack content)

-- Image
image :: Parser InlineText
image = do
  char '!'
  char '['
  alt <- T.pack <$> many (noneOf "]\n\r")
  char ']'
  (url, title) <- linkDestination
  return $ Image {altText = alt, url = url, title = title}

-- Link
link :: Parser InlineText
link = do
  char '['
  content <- some (notFollowedBy (char ']') >> inlineElementNoBracket)
  char ']'
  (url, title) <- linkDestination
  pure $ Link content url title

inlineElementNoBracket :: Parser InlineText
inlineElementNoBracket =
  choice
    [ try strong,
      try emphasis,
      try codeSpan,
      try htmlInline,
      try escapedChar,
      plainTextNoBracket
    ]

linkDestination :: Parser (Text, Maybe Text)
linkDestination = directLink <|> referenceLink
  where
    directLink = do
      char '('
      url <- many (noneOf " \t\n\r)")
      title <- optionMaybe (try $ some (char ' ' <|> char '\t') >> titleParser)
      char ')'
      pure (T.pack url, title)
    referenceLink = do
      char '['
      ref <- some (alphaNum <|> char ' ' <|> char '\t')
      char ']'
      -- For simplicity, we're not resolving references here
      -- In a real implementation, you'd look up the reference
      pure (T.pack $ "[" ++ ref ++ "]", Nothing)

titleParser :: Parser Text
titleParser =
  T.pack
    <$> choice
      [ between (char '"') (char '"') (many $ noneOf "\""),
        between (char '\'') (char '\'') (many $ noneOf "'"),
        between (char '(') (char ')') (many $ noneOf ")")
      ]

-- HTML Inline
htmlInline :: Parser InlineText
htmlInline = do
  start <- char '<'
  content <- manyTill anyChar (try $ char '>')
  return $ HTMLInline (T.pack (start : content ++ ">"))

-- Escaped Character
escapedChar :: Parser InlineText
escapedChar = do
  char '\\'
  c <- satisfy (\x -> x >= '!' && x <= '~')
  pure $ Text (T.singleton c)

-- Plain Text
-- TODO: this eats stuff it shouldn't, inefficient solution is to try other inline elements and exit if they succeed
plainText :: Parser InlineText
plainText = fmap (Text . T.pack) (liftA2 (:) (noneOf "\n") $ many plainTextChar)

plainTextChar :: Parser Char
plainTextChar = noneOf "\n[~`_*"

plainTextNoAsterisk :: Parser InlineText
plainTextNoAsterisk = fmap (Text . T.pack) $ some $ noneOf "*\n"

plainTextNoUnderscore :: Parser InlineText
plainTextNoUnderscore = fmap (Text . T.pack) $ some $ noneOf "_\n"

plainTextNoBracket :: Parser InlineText
plainTextNoBracket =
  fmap (Text . T.pack) $
    some $
      satisfy
        (`notElem` ("[]" :: String))

-- Helper Parsers
lineEnding :: Parser ()
lineEnding = void (try (string "\r\n") <|> try (string "\n") <|> string "\r")

wsParser :: Parser ()
wsParser = void $ some (char ' ' <|> char '\t')
