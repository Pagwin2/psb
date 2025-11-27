{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- (document, metadata)
module Markdown (document, metadata) where

import Control.Applicative (many, optional, some, (<|>))
import Control.Monad (guard, void)
import Data.Char (isAlpha)
import Data.Functor.Identity (Identity)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import IR
import Logger (Logger (logDebug))
import Text.Megaparsec (ParsecT, Stream, Token, Tokens, anySingle, anySingleBut, between, choice, count, eof, manyTill, notFollowedBy, satisfy, skipSome, try, (<?>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, string)
import Utilities (tee)

type ParserTG = ParsecT Void

type ParserT m = ParserTG T.Text m

type Parser = ParserT Identity

logP :: (Logger m, Show v, Token s ~ Char, Stream s) => ParserTG s m v -> ParserTG s m v
logP = tee (logDebug . T.show)

anyChar :: (Token s ~ Char, Stream s) => ParserTG s m Char
anyChar = anySingle

alphaNum :: (Token s ~ Char, Stream s) => ParserTG s m Char
alphaNum = alphaNumChar

digit :: (Token s ~ Char, Stream s) => ParserTG s m Char
digit = digitChar

noneOf :: (Token s ~ Char, Stream s) => [Char] -> ParserTG s m Char
noneOf = MP.noneOf

oneOf :: (Token s ~ Char, Stream s) => [Char] -> ParserTG s m Char
oneOf = MP.oneOf

optionMaybe :: (Token s ~ Char, Stream s) => ParserTG s m a -> ParserTG s m (Maybe a)
optionMaybe = optional

skipMany1 :: (Token s ~ Char, Stream s) => ParserTG s m a -> ParserTG s m ()
skipMany1 = skipSome

metadata :: (Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Text
metadata = bound *> fmap T.pack (many $ try (char '-' <* notFollowedBy (string "--")) <|> anySingleBut '-') <* bound
  where
    bound = string "---"

document :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Document
document = do
  logDebug "document"
  Doc <$> many element <* eof

element :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Element
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
blankLines :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Element
blankLines = do
  skipMany1 (blankLine *> notFollowedBy eof)
  element <|> fmap (const $ HTML HTMLTag {html_content = ""}) eof -- Parse the next element (or handle eof)

blankLine :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m ()
blankLine = do
  many (char ' ' <|> char '\t')
  lineEnding
  pure ()

-- Heading Block
headingBlock :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Element
headingBlock = do
  hashes <- some (char '#') <?> "Heading Hashes"
  let level = length hashes
  guard (level <= 6) <?> "Higher than level 6"
  many (char ' ' <|> char '\t') <?> "Pre-Text Whitespace"
  content <- manyTill (inlineElement <?> "Header Text") (try lineEnding <?> "Header Ending")
  pure $ Heading $ H level content

-- Fenced Code Block
fencedCodeBlock :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Element
fencedCodeBlock = do
  logDebug "fenced_coding_block"
  fence <- string "```" <|> string "~~~"
  logDebug "fence"
  lang <- optionMaybe languageInfo
  logDebug "langInfo"
  lineEnding'
  logDebug "lineEnding"
  codeLines <- manyTill (codeLine fence) (try $ string fence)
  logDebug "lines"
  pure $ Code $ C lang (T.pack $ unlines codeLines)

languageInfo :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Text
languageInfo = T.pack <$> some (alphaNum <|> char '-' <|> char '+' <|> char '.')

codeLine :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => (Tokens s) -> ParserTG s m String
codeLine fence = do
  -- this is a hack which can only haunt me if I continue using markdown
  line <- many $ (notFollowedBy $ string fence) *> noneOf "\n\r"
  lineEnding'
  pure line

-- Indented Code Block
indentedCodeBlock :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Element
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
blockquoteBlock :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Element
blockquoteBlock = do
  lines' <- some blockquoteLine
  pure $ BlockQuote $ Q (concat lines')
  where
    blockquoteLine = do
      char '>'
      optional (char ' ')
      content <- many $ notFollowedBy lineEnding' *> inlineElement
      pure content

-- Horizontal Rule Block
horizontalRuleBlock :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Element
horizontalRuleBlock = do
  choice
    [ try (count 3 (char '*') >> many (char ' ' <|> char '*')),
      try (count 3 (char '-') >> many (char ' ' <|> char '-')),
      try (count 3 (char '_') >> many (char ' ' <|> char '_'))
    ]
  lineEnding
  pure HorizontalRule

-- Unordered List Block
unorderedListBlock :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Element
unorderedListBlock = do
  items <- some unorderedListItem
  lineEnding'
  pure $ List $ L Unordered items

unorderedListItem :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m ListItem
unorderedListItem = do
  oneOf "*-+"
  char ' ' <|> char '\t'
  content <- many $ notFollowedBy lineEnding' *> inlineElement
  lineEnding'
  -- continuations <- many listContinuation
  children <- many (try indentedList)
  pure $ LI content children

listContinuation :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m [InlineText]
listContinuation = do
  count 2 (char ' ' <|> char '\t')
  many (char ' ' <|> char '\t')
  notFollowedBy (oneOf "*-+")
  notFollowedBy (digit >> char '.')
  content <- manyTill inlineElement (try lineEnding)
  pure content

-- TODO: handle list indentation at all levels
indentedList :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m List
indentedList = do
  let n = 1
  void $ count (4 * n) (char ' ') <|> count n (char '\t')
  choice [try indentedUnorderedList, indentedOrderedList]

indentedUnorderedList :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m List
indentedUnorderedList = do
  items <- some (try $ indentedListItem (oneOf "*-+" >> void (char ' ' <|> char '\t')))
  pure $ L Unordered items

indentedOrderedList :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m List
indentedOrderedList = do
  items <- some (try $ indentedListItem (some digit >> char '.' >> void (char ' ' <|> char '\t')))
  pure $ L Ordered items

indentedListItem :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m () -> ParserTG s m ListItem
indentedListItem marker = do
  marker
  content <- many $ notFollowedBy lineEnding' *> inlineElement
  pure $ LI content []

-- Ordered List Block
orderedListBlock :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Element
orderedListBlock = do
  items <- some orderedListItem
  lineEnding'
  pure $ List $ L Ordered items

orderedListItem :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m ListItem
orderedListItem = do
  some digit
  char '.' <|> char ')'
  optional (char ' ' <|> char '\t')
  content <- many $ notFollowedBy lineEnding' *> inlineElement
  -- continuations <- many listContinuation
  children <- many (try indentedList)
  pure $ LI content children

-- HTML Block
htmlBlock :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Element
htmlBlock = do
  char '<'
  -- Capture the entire HTML block as raw text
  rest <- manyTill anyChar (try $ char '>' >> lineEnding)
  let content = '<' : (rest <> ">")
  return $ HTML $ HTMLTag (T.pack content)

tagName :: (Logger m, Token s ~ Char, Stream s) => ParserTG s m String
tagName = do
  first <- satisfy isAlpha
  rest <- many (alphaNum <|> char '-' <|> char ':')
  pure (first : rest)

attribute :: (Logger m, Token s ~ Char, Stream s) => ParserTG s m (Text, Maybe Text)
attribute = do
  name <- attributeName
  value <- optionMaybe (char '=' >> attributeValue)
  pure (T.pack name, fmap T.pack value)

attributeName :: (Logger m, Token s ~ Char, Stream s) => ParserTG s m String
attributeName = do
  first <- satisfy isAlpha
  rest <- many (alphaNum <|> char '-' <|> char ':')
  pure (first : rest)

attributeValue :: (Logger m, Token s ~ Char, Stream s) => ParserTG s m String
attributeValue =
  choice
    [ between (char '"') (char '"') (many $ anySingleBut '"'),
      between (char '\'') (char '\'') (many $ anySingleBut '\''),
      some $ noneOf " \t\n\r>\"'=<`"
    ]

-- Paragraph Block
paragraphBlock :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m Element
paragraphBlock = do
  logDebug "paragraph"
  content <- some (notFollowedBy lineEnding *> inlineElement)
  lineEnding <|> eof
  pure $ Paragraph $ P content

-- Inline Elements
inlineElement :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
inlineElement =
  logDebug "inlineElement"
    *> choice
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
strong :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
strong = strongAsterisk <|> strongUnderscore

strongAsterisk :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
strongAsterisk = do
  string "**"
  content <- some (notFollowedBy (string "**") >> inlineElement)
  string "**"
  pure $ Bold content

strongUnderscore :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
strongUnderscore = do
  string "__"
  content <- some (notFollowedBy (string "__") >> inlineElement)
  string "__"
  pure $ Bold content

crossedText :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
crossedText = do
  string "~~"
  content <- some (notFollowedBy (string "~~") >> inlineElement)
  string "~~"
  pure $ Crossed content

-- Emphasis (Italic)
emphasis :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
emphasis = emphasisAsterisk <|> emphasisUnderscore

emphasisAsterisk :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
emphasisAsterisk = do
  char '*'
  content <- some (notFollowedBy (char '*') >> inlineElementNoAsterisk)
  char '*'
  pure $ Italic content

emphasisUnderscore :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
emphasisUnderscore = do
  char '_'
  content <- some inlineElementNoUnderscore
  char '_'
  pure $ Italic content

inlineElementNo :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => Char -> ParserTG s m InlineText
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

plainTextNo :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => [Char] -> ParserTG s m InlineText
plainTextNo list = do
  plainTextNo' False list

plainTextNo' :: (HasCallStack, Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => Bool -> [Char] -> ParserTG s m InlineText
plainTextNo' block_whitespace disallow = do
  logDebug $ "base plain Text: " <> T.show block_whitespace <> " " <> T.show disallow
  firstChar <- noneOf (disallow <> if block_whitespace then " \t\r\n" else []) <?> "Plain Text Initial Disallow"
  remChars <- many $ notFollowedBy lineEnding' *> plainTextCharNo disallow
  pure $ Text $ T.map wspHandler $ T.pack $ firstChar : remChars
  where
    wspHandler '\n' = ' '
    wspHandler c = c

inlineElementNoAsterisk :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
inlineElementNoAsterisk = inlineElementNo '*'

inlineElementNoUnderscore :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
inlineElementNoUnderscore = inlineElementNo '_'

-- Code Span
codeSpan :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
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
image :: (Logger m, Token s ~ Char, Stream s) => ParserTG s m InlineText
image = do
  char '!'
  char '['
  alt <- T.pack <$> many (noneOf "]\n\r")
  char ']'
  (url, title) <- linkDestination
  return $ Image {altText = alt, url = url, title = title}

-- Link
link :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
link = do
  char '['
  content <- some (notFollowedBy (char ']') >> inlineElementNoBracket)
  char ']'
  (url, title) <- linkDestination
  pure $ Link content url title

inlineElementNoBracket :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
inlineElementNoBracket =
  choice
    [ try strong,
      try emphasis,
      try codeSpan,
      try htmlInline,
      try escapedChar,
      plainTextNoBracket
    ]

linkDestination :: (Logger m, Token s ~ Char, Stream s) => ParserTG s m (Text, Maybe Text)
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

titleParser :: (Logger m, Token s ~ Char, Stream s) => ParserTG s m Text
titleParser =
  T.pack
    <$> choice
      [ between (char '"') (char '"') (many $ anySingleBut '"'),
        between (char '\'') (char '\'') (many $ anySingleBut '\''),
        between (char '(') (char ')') (many $ anySingleBut ')')
      ]

-- HTML Inline
htmlInline :: (Logger m, Token s ~ Char, Stream s) => ParserTG s m InlineText
htmlInline = do
  start <- char '<'
  content <- manyTill anyChar (try $ char '>')
  return $ HTMLInline (T.pack (start : content ++ ">"))

-- Escaped Character
escapedChar :: (Logger m, Token s ~ Char, Stream s) => ParserTG s m InlineText
escapedChar = do
  char '\\'
  c <- satisfy (\x -> x >= '!' && x <= '~')
  pure $ Text (T.singleton c)

-- Plain Text
plainText :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
plainText = plainTextNo' False [] <?> "Baseline Plain Text"

plainTextBaseDisallow :: [Char]
plainTextBaseDisallow = "[~`_*<"

plainTextCharNo :: (Logger m, Token s ~ Char, Stream s) => [Char] -> ParserTG s m Char
plainTextCharNo additional = noneOf $ additional <> plainTextBaseDisallow

plainTextNoAsterisk :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
plainTextNoAsterisk = plainTextNo "*"

plainTextNoUnderscore :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
plainTextNoUnderscore = plainTextNo "_"

plainTextNoBracket :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m InlineText
plainTextNoBracket = plainTextNo "[]"

-- Helper Parsers
lineEnding :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m ()
lineEnding = void (try $ count 2 (try (string "\r\n") <|> try (string "\n") <|> string "\r")) <|> eof

lineEnding' :: (Logger m, Token s ~ Char, Stream s, IsString (Tokens s)) => ParserTG s m ()
lineEnding' = void (try (string "\r\n") <|> try (string "\n") <|> string "\r") <|> eof

wsParser :: (Logger m, Token s ~ Char, Stream s) => ParserTG s m ()
wsParser = void $ some (char ' ' <|> char '\t')
