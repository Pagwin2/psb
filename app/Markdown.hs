{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- (document, metadata)
module Markdown where

import Control.Applicative (many, optional, some, (<|>))
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import IR
import Text.Megaparsec (ParsecT, anySingle, anySingleBut, between, choice, count, eof, manyTill, notFollowedBy, satisfy, skipSome, try, (<?>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, string)

type ParserT m = ParsecT Void String m

type Parser = ParserT IO

log_ :: String -> Parser ()
log_ = lift . putStrLn

logP :: (Show s) => Parser s -> Parser s
logP v = do
  underlying <- v
  log_ $ show underlying
  v

anyChar :: Parser Char
anyChar = anySingle

alphaNum :: Parser Char
alphaNum = alphaNumChar

digit :: Parser Char
digit = digitChar

noneOf :: [Char] -> Parser Char
noneOf = MP.noneOf

oneOf :: [Char] -> Parser Char
oneOf = MP.oneOf

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe = optional

skipMany1 :: Parser a -> Parser ()
skipMany1 = skipSome

metadata :: Parser Text
metadata = bound *> fmap T.pack (many $ try (char '-' <* notFollowedBy (string "--")) <|> anySingleBut '-') <* bound
  where
    bound = string "---"

document :: Parser Document
document = Doc <$> many element <* eof

element :: Parser Element
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
blankLines :: Parser Element
blankLines = do
  skipMany1 (blankLine *> notFollowedBy eof)
  element <|> fmap (const $ HTML HTMLTag {html_content = ""}) eof -- Parse the next element (or handle eof)

blankLine :: Parser ()
blankLine = do
  many (char ' ' <|> char '\t')
  lineEnding
  pure ()

-- Heading Block
headingBlock :: Parser Element
headingBlock = do
  hashes <- some (char '#') <?> "Heading Hashes"
  let level = length hashes
  guard (level <= 6) <?> "Higher than level 6"
  many (char ' ' <|> char '\t') <?> "Pre-Text Whitespace"
  log_ "heading content start"
  content <- manyTill (inlineElement <?> "Header Text") (log_ "attempt" *> try lineEnding <?> "Header Ending")
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
languageInfo = T.pack <$> some (alphaNum <|> char '-' <|> char '+' <|> char '.')

codeLine :: Parser String
codeLine = do
  line <- many $ noneOf "\n\r"
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
      line <- many $ noneOf "\n\r"
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
    [ between (char '"') (char '"') (many $ anySingleBut '"'),
      between (char '\'') (char '\'') (many $ anySingleBut '\''),
      some $ noneOf " \t\n\r>\"'=<`"
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
  log_ "inline element call"
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
  content <- some (notFollowedBy (string "~~") >> inlineElement)
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
      plainTextNo [c]
    ]

plainTextNo :: [Char] -> Parser InlineText
plainTextNo disallow = do
  log_ "a"
  firstChar <- noneOf disallow <?> "Plain Text Initial Disallow"
  log_ "b"
  remChars <- manyTill (plainTextCharNo disallow) lineEnding <?> "Remaining Characters"
  pure $ Text $ T.map wspHandler $ T.pack $ firstChar : remChars
  where
    wspHandler '\n' = ' '
    wspHandler c = c

inlineElementNoAsterisk :: Parser InlineText
inlineElementNoAsterisk = inlineElementNo '*'

inlineElementNoUnderscore :: Parser InlineText
inlineElementNoUnderscore = inlineElementNo '_'

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

titleParser :: Parser Text
titleParser =
  T.pack
    <$> choice
      [ between (char '"') (char '"') (many $ anySingleBut '"'),
        between (char '\'') (char '\'') (many $ anySingleBut '\''),
        between (char '(') (char ')') (many $ anySingleBut ')')
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
plainText :: Parser InlineText
plainText = plainTextNo [] <?> "Baseline Plain Text"

plainTextBaseDisallow :: [Char]
plainTextBaseDisallow = "[~`_*<"

plainTextCharNo :: [Char] -> Parser Char
plainTextCharNo additional = noneOf $ additional <> plainTextBaseDisallow

plainTextNoAsterisk :: Parser InlineText
plainTextNoAsterisk = plainTextNo "*"

plainTextNoUnderscore :: Parser InlineText
plainTextNoUnderscore = plainTextNo "_"

plainTextNoBracket :: Parser InlineText
plainTextNoBracket = plainTextNo "[]"

-- Helper Parsers
lineEnding :: Parser ()
lineEnding = void (try $ count 2 (try (string "\r\n") <|> try (string "\n") <|> string "\r")) <|> eof

wsParser :: Parser ()
wsParser = void $ some (char ' ' <|> char '\t')
