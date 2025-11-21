{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- (document, metadata)
module Markdown where

import Control.Applicative (many, optional, some, (<|>))
import Control.Monad (guard, void)
import Data.Char (isAlpha)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import IR
import Logger (Logger (logDebug))
import Text.Megaparsec (ParsecT, anySingle, anySingleBut, between, choice, count, eof, manyTill, notFollowedBy, satisfy, skipSome, try, (<?>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, string)

type ParserT m = ParsecT Void String m

type Parser = ParserT Identity

logP :: (Logger m, Show s) => ParserT m s -> ParserT m s
logP v = do
  underlying <- v
  logDebug $ T.show underlying
  v

anyChar :: ParserT m Char
anyChar = anySingle

alphaNum :: ParserT m Char
alphaNum = alphaNumChar

digit :: ParserT m Char
digit = digitChar

noneOf :: [Char] -> ParserT m Char
noneOf = MP.noneOf

oneOf :: [Char] -> ParserT m Char
oneOf = MP.oneOf

optionMaybe :: ParserT m a -> ParserT m (Maybe a)
optionMaybe = optional

skipMany1 :: ParserT m a -> ParserT m ()
skipMany1 = skipSome

metadata :: ParserT m Text
metadata = bound *> fmap T.pack (many $ try (char '-' <* notFollowedBy (string "--")) <|> anySingleBut '-') <* bound
  where
    bound = string "---"

document :: (Logger m) => ParserT m Document
document = Doc <$> many element <* eof

element :: (Logger m) => ParserT m Element
element =
  choice
    [ try headingBlock <?> "Element Heading",
      try fencedCodeBlock <?> "Fenced Code Block",
      try indentedCodeBlock <?> "Indented Code Block",
      try blockquoteBlock <?> "BlockQuote",
      try unorderedListBlock <?> "Unordered List",
      try orderedListBlock <?> "Ordered List",
      try horizontalRuleBlock <?> "Horizontal Rule",
      try htmlBlock <?> "HTML Block",
      try blankLines <?> "Blank Lines", -- Consume blank lines but don't add to AST
      paragraphBlock <?> "Paragarph"
    ]

-- Blank lines (consumed but not stored)
blankLines :: (Logger m) => ParserT m Element
blankLines = do
  skipMany1 (blankLine *> notFollowedBy eof)
  element <|> fmap (const $ HTML HTMLTag {html_content = ""}) eof -- Parse the next element (or handle eof)

blankLine :: (Logger m) => ParserT m ()
blankLine = do
  many (char ' ' <|> char '\t')
  lineEnding
  pure ()

-- Heading Block
headingBlock :: (Logger m) => ParserT m Element
headingBlock = do
  hashes <- some (char '#') <?> "Heading Hashes"
  let level = length hashes
  guard (level <= 6) <?> "Higher than level 6"
  many (char ' ' <|> char '\t') <?> "Pre-Text Whitespace"
  content <- manyTill (inlineElement <?> "Header Text") (try lineEnding <?> "Header Ending")
  pure $ Heading $ H level content

-- Fenced Code Block
fencedCodeBlock :: (Logger m) => ParserT m Element
fencedCodeBlock = do
  fence <- string "```" <|> string "~~~"
  lang <- optionMaybe languageInfo
  lineEnding
  codeLines <- manyTill codeLine (try $ string fence)
  pure $ Code $ C lang (T.pack $ unlines codeLines)

languageInfo :: (Logger m) => ParserT m Text
languageInfo = T.pack <$> some (alphaNum <|> char '-' <|> char '+' <|> char '.')

codeLine :: (Logger m) => ParserT m String
codeLine = do
  line <- many $ noneOf "\n\r"
  lineEnding
  pure line

-- Indented Code Block
indentedCodeBlock :: (Logger m) => ParserT m Element
indentedCodeBlock = do
  lines' <- some indentedLine
  pure $ Code $ C Nothing (T.pack $ unlines lines')
  where
    indentedLine = do
      count 4 (char ' ' <|> char '\t')
      line <- many $ noneOf "\n\r"
      lineEnding
      pure line

-- Blockquote Block
blockquoteBlock :: (Logger m) => ParserT m Element
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
horizontalRuleBlock :: (Logger m) => ParserT m Element
horizontalRuleBlock = do
  choice
    [ try (count 3 (char '*') >> many (char ' ' <|> char '*')),
      try (count 3 (char '-') >> many (char ' ' <|> char '-')),
      try (count 3 (char '_') >> many (char ' ' <|> char '_'))
    ]
  lineEnding
  pure HorizontalRule

-- Unordered List Block
unorderedListBlock :: (Logger m) => ParserT m Element
unorderedListBlock = do
  items <- some unorderedListItem
  pure $ List $ L Unordered items

unorderedListItem :: (Logger m) => ParserT m ListItem
unorderedListItem = do
  oneOf "*-+"
  char ' ' <|> char '\t'
  content <- manyTill inlineElement (try lineEnding)
  -- continuations <- many listContinuation
  children <- many (try indentedList)
  pure $ LI content children

listContinuation :: (Logger m) => ParserT m [InlineText]
listContinuation = do
  count 2 (char ' ' <|> char '\t')
  many (char ' ' <|> char '\t')
  notFollowedBy (oneOf "*-+")
  notFollowedBy (digit >> char '.')
  content <- manyTill inlineElement (try lineEnding)
  pure content

-- TODO: handle list indentation at all levels
indentedList :: (Logger m) => ParserT m List
indentedList = do
  let n = 1
  void $ count (4 * n) (char ' ') <|> count n (char '\t')
  choice [try indentedUnorderedList, indentedOrderedList]

indentedUnorderedList :: (Logger m) => ParserT m List
indentedUnorderedList = do
  items <- some (try $ indentedListItem (oneOf "*-+" >> void (char ' ' <|> char '\t')))
  pure $ L Unordered items

indentedOrderedList :: (Logger m) => ParserT m List
indentedOrderedList = do
  items <- some (try $ indentedListItem (some digit >> char '.' >> void (char ' ' <|> char '\t')))
  pure $ L Ordered items

indentedListItem :: (Logger m) => ParserT m () -> ParserT m ListItem
indentedListItem marker = do
  marker
  content <- manyTill inlineElement (try $ lineEnding <|> eof)
  pure $ LI content []

-- Ordered List Block
orderedListBlock :: (Logger m) => ParserT m Element
orderedListBlock = do
  items <- some orderedListItem
  pure $ List $ L Ordered items

orderedListItem :: (Logger m) => ParserT m ListItem
orderedListItem = do
  some digit
  char '.'
  char ' ' <|> char '\t'
  content <- manyTill inlineElement (try lineEnding)
  continuations <- many listContinuation
  children <- many (try indentedList)
  pure $ LI (content ++ concat continuations) children

-- HTML Block
htmlBlock :: (Logger m) => ParserT m Element
htmlBlock = do
  char '<'
  -- Capture the entire HTML block as raw text
  rest <- manyTill anyChar (try $ char '>' >> lineEnding)
  let content = '<' : rest
  return $ HTML $ HTMLTag (T.pack content)

tagName :: (Logger m) => ParserT m String
tagName = do
  first <- satisfy isAlpha
  rest <- many (alphaNum <|> char '-' <|> char ':')
  pure (first : rest)

attribute :: (Logger m) => ParserT m (Text, Maybe Text)
attribute = do
  name <- attributeName
  value <- optionMaybe (char '=' >> attributeValue)
  pure (T.pack name, fmap T.pack value)

attributeName :: (Logger m) => ParserT m String
attributeName = do
  first <- satisfy isAlpha
  rest <- many (alphaNum <|> char '-' <|> char ':')
  pure (first : rest)

attributeValue :: (Logger m) => ParserT m String
attributeValue =
  choice
    [ between (char '"') (char '"') (many $ anySingleBut '"'),
      between (char '\'') (char '\'') (many $ anySingleBut '\''),
      some $ noneOf " \t\n\r>\"'=<`"
    ]

-- Paragraph Block
paragraphBlock :: (Logger m) => ParserT m Element
paragraphBlock = do
  content <- some inlineElement
  lineEnding <|> eof
  pure $ Paragraph $ P content

-- Inline Elements
inlineElement :: (Logger m) => ParserT m InlineText
inlineElement =
  choice
    [ try strong <?> "Inline Strong Text",
      try emphasis <?> "Inline Italic Text",
      try crossedText <?> "Inline Crossed Text",
      try codeSpan <?> "Inline Code",
      try image <?> "Inline Image",
      try link <?> "Inline Link",
      try htmlInline <?> "Inline HTML",
      try escapedChar <?> "Escaped Character",
      plainText <?> "Inline Plain Text"
    ]

-- Strong (Bold)
strong :: (Logger m) => ParserT m InlineText
strong = strongAsterisk <|> strongUnderscore

strongAsterisk :: (Logger m) => ParserT m InlineText
strongAsterisk = do
  string "**"
  content <- some (notFollowedBy (string "**") >> inlineElement)
  string "**"
  pure $ Bold content

strongUnderscore :: (Logger m) => ParserT m InlineText
strongUnderscore = do
  string "__"
  content <- some (notFollowedBy (string "__") >> inlineElement)
  string "__"
  pure $ Bold content

crossedText :: (Logger m) => ParserT m InlineText
crossedText = do
  string "~~"
  content <- some (notFollowedBy (string "~~") >> inlineElement)
  string "~~"
  pure $ Crossed content

-- Emphasis (Italic)
emphasis :: (Logger m) => ParserT m InlineText
emphasis = emphasisAsterisk <|> emphasisUnderscore

emphasisAsterisk :: (Logger m) => ParserT m InlineText
emphasisAsterisk = do
  char '*'
  content <- some (notFollowedBy (char '*') >> inlineElementNoAsterisk)
  char '*'
  pure $ Italic content

emphasisUnderscore :: (Logger m) => ParserT m InlineText
emphasisUnderscore = do
  char '_'
  content <- some inlineElementNoUnderscore
  char '_'
  pure $ Italic content

inlineElementNo :: (Logger m) => Char -> ParserT m InlineText
inlineElementNo c =
  choice
    [ try strong,
      try codeSpan,
      try image,
      try link,
      try htmlInline,
      try escapedChar,
      plainTextNo [c]
    ]

plainTextNo :: (Logger m) => [Char] -> ParserT m InlineText
plainTextNo = plainTextNo' False

plainTextNo' :: (Logger m) => Bool -> [Char] -> ParserT m InlineText
plainTextNo' block_whitespace disallow = do
  firstChar <- noneOf (disallow <> if block_whitespace then " \t\r\n" else []) <?> "Plain Text Initial Disallow"
  remChars <- many $ notFollowedBy lineEnding *> plainTextCharNo disallow
  pure $ Text $ T.map wspHandler $ T.pack $ firstChar : remChars
  where
    wspHandler '\n' = ' '
    wspHandler c = c

inlineElementNoAsterisk :: (Logger m) => ParserT m InlineText
inlineElementNoAsterisk = inlineElementNo '*'

inlineElementNoUnderscore :: (Logger m) => ParserT m InlineText
inlineElementNoUnderscore = inlineElementNo '_'

-- Code Span
codeSpan :: (Logger m) => ParserT m InlineText
codeSpan =
  choice
    [ try tripleBacktick,
      try doubleBacktick,
      singleBacktick
    ]
  where
    singleBacktick = do
      char '`'
      content <- many $ noneOf "`\n\r"
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
image :: (Logger m) => ParserT m InlineText
image = do
  char '!'
  char '['
  alt <- T.pack <$> many (noneOf "]\n\r")
  char ']'
  (url, title) <- linkDestination
  return $ Image {altText = alt, url = url, title = title}

-- Link
link :: (Logger m) => ParserT m InlineText
link = do
  char '['
  content <- some (notFollowedBy (char ']') >> inlineElementNoBracket)
  char ']'
  (url, title) <- linkDestination
  pure $ Link content url title

inlineElementNoBracket :: (Logger m) => ParserT m InlineText
inlineElementNoBracket =
  choice
    [ try strong,
      try emphasis,
      try codeSpan,
      try htmlInline,
      try escapedChar,
      plainTextNoBracket
    ]

linkDestination :: (Logger m) => ParserT m (Text, Maybe Text)
linkDestination = directLink <|> referenceLink
  where
    directLink = do
      char '('
      url <- many $ noneOf " \t\n\r)"
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

titleParser :: (Logger m) => ParserT m Text
titleParser =
  T.pack
    <$> choice
      [ between (char '"') (char '"') (many $ anySingleBut '"'),
        between (char '\'') (char '\'') (many $ anySingleBut '\''),
        between (char '(') (char ')') (many $ anySingleBut ')')
      ]

-- HTML Inline
htmlInline :: (Logger m) => ParserT m InlineText
htmlInline = do
  start <- char '<'
  content <- manyTill anyChar (try $ char '>')
  return $ HTMLInline (T.pack (start : content ++ ">"))

-- Escaped Character
escapedChar :: (Logger m) => ParserT m InlineText
escapedChar = do
  char '\\'
  c <- satisfy (\x -> x >= '!' && x <= '~')
  pure $ Text (T.singleton c)

-- Plain Text
plainText :: (Logger m) => ParserT m InlineText
plainText = plainTextNo' True [] <?> "Baseline Plain Text"

plainTextBaseDisallow :: [Char]
plainTextBaseDisallow = "[~`_*<"

plainTextCharNo :: (Logger m) => [Char] -> ParserT m Char
plainTextCharNo additional = noneOf $ additional <> plainTextBaseDisallow

plainTextNoAsterisk :: (Logger m) => ParserT m InlineText
plainTextNoAsterisk = plainTextNo "*"

plainTextNoUnderscore :: (Logger m) => ParserT m InlineText
plainTextNoUnderscore = plainTextNo "_"

plainTextNoBracket :: (Logger m) => ParserT m InlineText
plainTextNoBracket = plainTextNo "[]"

-- Helper Parsers
lineEnding :: (Logger m) => ParserT m ()
lineEnding = void (try $ count 2 (try (string "\r\n") <|> try (string "\n") <|> string "\r")) <|> eof

wsParser :: (Logger m) => ParserT m ()
wsParser = void $ some (char ' ' <|> char '\t')
