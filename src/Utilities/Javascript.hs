{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utilities.Javascript
  ( minify,
    toTokens,
    displayToken,
  )
where

import Control.Applicative (Alternative (many), optional, (<|>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, evalStateT, put)
import Data.Data (Proxy (Proxy))
import Data.Functor ((<&>))
import Data.Maybe (maybeToList)
import Data.String (IsString (fromString))
import Data.Void (Void)
import Logger
import Text.Megaparsec (MonadParsec (notFollowedBy, try), ParseErrorBundle, ParsecT, Stream (tokensToChunk), anySingle, choice, parse, runParserT)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, digitChar, eol, hspace, letterChar, newline, string)
import Utilities.Parsing (Characters, ToText (fromText, toString, toText))

data Possibility = ExprAllowed | ExprNotAllowed deriving (Eq)

type Parser s m = ParsecT Void s (StateT Possibility m)

minify :: (Characters s) => [Token s] -> [Token s]
minify = reduce_identifiers . remove_redundants
  where
    -- need to figure out how to add State into this
    reduce_identifiers = map $ \token -> case token of
      Identifier name -> Identifier name
      v -> v
    -- this could also use state so I can remove redundant newlines
    remove_redundants = filter $ \token -> case token of
      WhiteSpace -> False
      _ -> True

toTokens :: (Characters s, Logger m) => String -> s -> m (Either (ParseErrorBundle s Void) [Token s])
toTokens src stream = evalStateT (runParserT tokens src stream) ExprAllowed

displayToken :: (ToText s) => Token s -> s
displayToken WhiteSpace = fromText " "
displayToken LineTerminator = fromText "\n"
displayToken (Identifier i) = i
displayToken (HashBangComment text) = fromText ("#!" <> toText text)
displayToken (SingleLineComment text) = fromText ("//" <> toText text)
displayToken (MultiLineComment text) = fromText ("/*" <> toText text <> "*/")
displayToken (PrivateIdentifier i) = fromText ("#" <> toText i)
displayToken (ReservedWord r) = displayReserved r
displayToken (Literal l) = displayLiteral l
displayToken (Punc p) = displayPunc p

displayReserved :: (ToText s) => Reserved -> s
displayReserved Await = fromText "await"
displayReserved Break = fromText "break"
displayReserved Case = fromText "case"
displayReserved Catch = fromText "catch"
displayReserved Class = fromText "class"
displayReserved Const = fromText "const"
displayReserved Continue = fromText "continue"
displayReserved Debugger = fromText "debugger"
displayReserved Default = fromText "default"
displayReserved Delete = fromText "delete"
displayReserved Do = fromText "do"
displayReserved Else = fromText "else"
displayReserved Enum = fromText "enum"
displayReserved Export = fromText "export"
displayReserved Extends = fromText "extends"
displayReserved FalseVal = fromText "false"
displayReserved Finally = fromText "finally"
displayReserved For = fromText "for"
displayReserved Function = fromText "function"
displayReserved If = fromText "if"
displayReserved Import = fromText "import"
displayReserved In = fromText "in"
displayReserved Instanceof = fromText "instanceof"
displayReserved New = fromText "new"
displayReserved Null = fromText "null"
displayReserved Return = fromText "return"
displayReserved Super = fromText "super"
displayReserved Switch = fromText "switch"
displayReserved This = fromText "this"
displayReserved Throw = fromText "throw"
displayReserved TrueVal = fromText "true"
displayReserved Try = fromText "try"
displayReserved Typeof = fromText "typeof"
displayReserved Var = fromText "var"
displayReserved Void = fromText "void"
displayReserved While = fromText "while"
displayReserved With = fromText "with"
displayReserved Yield = fromText "yield"

displayLiteral :: (ToText s) => Literal s -> s
displayLiteral (Number num) = num
displayLiteral (String s) = fromText $ "\"" <> toText s <> "\""
displayLiteral (Regex {body, flags}) = fromText $ "/" <> toText body <> "/" <> toText flags
displayLiteral (TemplateFragment frag) = displayTemplateFrag frag

displayTemplateFrag :: (ToText s) => TemplateFragment s -> s
displayTemplateFrag (NoSub s) = fromText $ "`" <> toText s <> "`"
displayTemplateFrag (TemplateHead s) = fromText $ "`" <> toText s <> "${"
displayTemplateFrag (TemplateMiddle s) = fromText $ "}" <> toText s <> "${"
displayTemplateFrag (TemplateTail s) = fromText $ "}" <> toText s <> "`"

displayPunc :: (ToText s) => Punctuator -> s
displayPunc Add = fromText "+"
displayPunc Sub = fromText "-"
displayPunc Mult = fromText "*"
displayPunc Div = fromText "/"
displayPunc Mod = fromText "%"
displayPunc Exp = fromText "**"
displayPunc Inc = fromText "++"
displayPunc Dec = fromText "--"
displayPunc Utilities.Javascript.LT = fromText "<"
displayPunc Utilities.Javascript.GT = fromText ">"
displayPunc LTEQ = fromText "<="
displayPunc GTEQ = fromText ">="
displayPunc DoubleEqual = fromText "=="
displayPunc NotEqual = fromText "!="
displayPunc TripleEqual = fromText "==="
displayPunc DoubleNotEqual = fromText "!=="
displayPunc LeftShift = fromText "<<"
displayPunc RightShift = fromText ">>"
displayPunc UnsignedRightShift = fromText ">>>"
displayPunc BitwiseAnd = fromText "&"
displayPunc BitwiseOr = fromText "|"
displayPunc BitwiseXor = fromText "^"
displayPunc BitwiseNot = fromText "~"
displayPunc LogicalAnd = fromText "&&"
displayPunc LogicalOr = fromText "||"
displayPunc LogicalNot = fromText "!"
displayPunc Nullish = fromText "??"
displayPunc Assign = fromText "="
displayPunc AddAssign = fromText "+="
displayPunc SubAssign = fromText "-="
displayPunc MultAssign = fromText "*="
displayPunc DivAssign = fromText "/="
displayPunc ModAssign = fromText "%="
displayPunc ExpAssign = fromText "**="
displayPunc LeftShiftAssign = fromText "<<="
displayPunc RightShiftAssign = fromText ">>="
displayPunc UnsignedRightShiftAssign = fromText ">>>="
displayPunc BitwiseAndAssign = fromText "&="
displayPunc BitwiseOrAssign = fromText "|="
displayPunc BitwiseXorAssign = fromText "^="
displayPunc LogicalAndAssign = fromText "&&="
displayPunc LogicalOrAssign = fromText "||="
displayPunc NullishAssign = fromText "??="
displayPunc LParen = fromText "("
displayPunc RParen = fromText ")"
displayPunc LCurly = fromText "{"
displayPunc RCurly = fromText "}"
displayPunc LSquare = fromText "["
displayPunc RSquare = fromText "]"
displayPunc Dot = fromText "."
displayPunc Spread = fromText "..."
displayPunc Semicolon = fromText ";"
displayPunc Comma = fromText ","
displayPunc OptionalChain = fromText "?."

-- yeah I guess I'm making a javascript tokenizer
-- s is either Text or String
-- Regex will be tokenized
data Token s
  = WhiteSpace
  | LineTerminator
  | SingleLineComment s
  | MultiLineComment s
  | HashBangComment s
  | Identifier s
  | PrivateIdentifier s
  | ReservedWord Reserved
  | Literal (Literal s)
  | Punc Punctuator
  deriving (Eq)

data Reserved = Await | Break | Case | Catch | Class | Const | Continue | Debugger | Default | Delete | Do | Else | Enum | Export | Extends | FalseVal | Finally | For | Function | If | Import | In | Instanceof | New | Null | Return | Super | Switch | This | Throw | TrueVal | Try | Typeof | Var | Void | While | With | Yield deriving (Eq)

data Literal s = Number s | String s | Regex {body :: s, flags :: s} | TemplateFragment (TemplateFragment s) deriving (Eq)

data TemplateFragment s = NoSub s | TemplateHead s | TemplateMiddle s | TemplateTail s deriving (Eq)

data Punctuator = Add | Sub | Mult | Div | Mod | Exp | Inc | Dec | LT | GT | LTEQ | GTEQ | DoubleEqual | NotEqual | TripleEqual | DoubleNotEqual | LeftShift | RightShift {- >>> -} | UnsignedRightShift | BitwiseAnd | BitwiseOr | BitwiseXor | BitwiseNot | LogicalAnd | LogicalOr | LogicalNot {- ?? -} | Nullish | Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign | ExpAssign | LeftShiftAssign | RightShiftAssign | UnsignedRightShiftAssign | BitwiseAndAssign | BitwiseOrAssign | BitwiseXorAssign | LogicalAndAssign | LogicalOrAssign | NullishAssign | LParen | RParen | LCurly | RCurly | LSquare | RSquare | Dot | Spread | Semicolon | Comma | OptionalChain deriving (Eq)

tokens :: (Logger m, Characters s) => Parser s m [Token s]
tokens = do
  hashbang <- (optional hashbang_comment)
  tokens <- many token
  pure $ (maybeToList hashbang) ++ tokens

exprAllowed :: (Stream s, Monad m) => Parser s m ()
exprAllowed = lift $ put ExprAllowed

exprNotAllowed :: (Stream s, Monad m) => Parser s m ()
exprNotAllowed = lift $ put ExprNotAllowed

exprNoop :: (Stream s, Monad m) => String -> Parser s m ()
-- string arg is just as a comment
exprNoop _ = pure ()

-- TODO: read https://github.com/jquery/esprima/blob/main/src/scanner.ts
-- and https://github.com/acornjs/acorn/blob/master/acorn/src/tokenize.js
-- specific logic at https://github.com/acornjs/acorn/blob/54097dcf8c08733695df7168692d0faac3a2f768/acorn/src/tokencontext.js#L92
-- https://astexplorer.net/
token :: (Logger m, Characters s) => Parser s m (Token s)
token =
  choice
    [ try comment
        <* exprNoop "comments don't change whether an expression is allowed or not",
      try reserved_word
        <* exprNoop "each reserved word can be different so it needs to be handled in there",
      -- discovered when I realized yield can be an identifier and doesn't let you get away with nonsense
      try identifier <* exprNotAllowed,
      -- Assuming it's the same as identifier
      try private_identifier <* exprNotAllowed,
      -- briefly was concerned about {} but then realized that isn't a literal
      try literal <* exprNotAllowed,
      -- handled on a case by case basis
      try punctuator <* error "TODO",
      try linebreak <* exprNoop "technically wrong due to semicolon insertion but hopefully that never comes up usage for this",
      whitespace <* exprNoop "non linebreak whitespace doesn't change whether an expression is allowed or not, same as comments"
    ]

hashbang_comment :: (Logger m, Characters s) => Parser s m (Token s)
hashbang_comment = do
  string "#!"
  text <- many ((notFollowedBy newline) *> anySingle)
  pure $ HashBangComment $ fromString text

comment :: (Logger m, Characters s) => Parser s m (Token s)
comment = (try singleline_com) <|> multiline_com
  where
    singleline_com = do
      string "//"
      text <- many ((notFollowedBy newline) *> anySingle)
      pure $ SingleLineComment $ fromString text
    multiline_com = do
      string "/*"
      text <- many ((notFollowedBy $ string "*/") *> anySingle)
      pure $ MultiLineComment $ fromString text

reserved_word :: (Logger m, Characters s) => Parser s m (Token s)
reserved_word =
  choice
    [ try await <* exprAllowed,
      try break <* error "TODO exprAllowed",
      try case_ <* exprAllowed,
      try catch_ <* error "TODO exprAllowed",
      try class_ <* error "TODO exprAllowed",
      try const <* error "TODO exprAllowed",
      try continue <* error "TODO exprAllowed",
      try debugger <* error "TODO exprAllowed",
      try default_ <* error "TODO exprAllowed",
      try delete <* exprAllowed,
      try do_ <* error "TODO exprAllowed",
      try else_ <* error "TODO exprAllowed",
      try enum <* error "TODO exprAllowed",
      try export <* error "TODO exprAllowed",
      try extends <* error "TODO exprAllowed",
      try false <* exprNotAllowed,
      try finally_ <* error "TODO exprAllowed",
      try for_ <* error "TODO exprAllowed",
      try function <* error "TODO exprAllowed",
      try if_ <* error "TODO exprAllowed",
      try import_ <* error "TODO exprAllowed",
      try in_ <* exprAllowed,
      try instanceof <* exprAllowed,
      try new <* error "TODO exprAllowed",
      try null <* error "TODO exprAllowed",
      try return <* exprAllowed,
      try super <* error "TODO exprAllowed",
      try switch <* error "TODO exprAllowed",
      try this <* error "TODO exprAllowed",
      try throw_ <* exprAllowed,
      try true <* exprNotAllowed,
      try try_ <* error "TODO exprAllowed",
      try typeof <* exprAllowed,
      try var <* exprNotAllowed,
      try void <* error "TODO exprAllowed",
      try while <* error "TODO exprAllowed",
      try with <* error "TODO exprAllowed",
      yield <* exprNotAllowed
    ]
  where
    await = string "await" *> pure (ReservedWord Await)
    break = string "break" *> pure (ReservedWord Break)
    case_ = string "case" *> pure (ReservedWord Case)
    catch_ = string "catch" *> pure (ReservedWord Catch)
    class_ = string "class" *> pure (ReservedWord Class)
    const = string "const" *> pure (ReservedWord Const)
    continue = string "continue" *> pure (ReservedWord Continue)
    debugger = string "debugger" *> pure (ReservedWord Debugger)
    default_ = string "default" *> pure (ReservedWord Default)
    delete = string "delete" *> pure (ReservedWord Delete)
    do_ = string "do" *> pure (ReservedWord Do)
    else_ = string "else" *> pure (ReservedWord Else)
    enum = string "enum" *> pure (ReservedWord Enum)
    export = string "export" *> pure (ReservedWord Export)
    extends = string "extends" *> pure (ReservedWord Extends)
    false = string "false" *> pure (ReservedWord FalseVal)
    finally_ = string "finally" *> pure (ReservedWord Finally)
    for_ = string "for" *> pure (ReservedWord For)
    function = string "function" *> pure (ReservedWord Function)
    if_ = string "if" *> pure (ReservedWord If)
    import_ = string "import" *> pure (ReservedWord Import)
    in_ = string "in" *> pure (ReservedWord In)
    instanceof = string "instanceof" *> pure (ReservedWord Instanceof)
    new = string "new" *> pure (ReservedWord New)
    null = string "null" *> pure (ReservedWord Null)
    return = string "return" *> pure (ReservedWord Return)
    super = string "super" *> pure (ReservedWord Super)
    switch = string "switch" *> pure (ReservedWord Switch)
    this = string "this" *> pure (ReservedWord This)
    throw_ = string "throw" *> pure (ReservedWord Throw)
    true = string "true" *> pure (ReservedWord TrueVal)
    try_ = string "try" *> pure (ReservedWord Try)
    typeof = string "typeof" *> pure (ReservedWord Typeof)
    var = string "var" *> pure (ReservedWord Var)
    void = string "void" *> pure (ReservedWord Void)
    while = string "while" *> pure (ReservedWord While)
    with = string "with" *> pure (ReservedWord With)
    yield = string "yield" *> pure (ReservedWord Yield)

identifier :: forall s m. (Logger m, Characters s) => Parser s m (Token s)
identifier = do
  first <- start_char
  rem <- many rem_char
  pure $ Identifier $ fromString (first : rem)
  where
    start_char :: Parser s m (MP.Token s)
    start_char = (char '$') <|> char '_' <|> letterChar
    rem_char :: Parser s m (MP.Token s)
    rem_char = start_char <|> digitChar

private_identifier :: (Logger m, Characters s) => Parser s m (Token s)
private_identifier =
  char '#'
    *> identifier
    <&> \(Identifier i) -> PrivateIdentifier i

literal :: forall s m. (Logger m, Characters s) => Parser s m (Token s)
literal =
  Literal
    <$> ( choice
            [ try template_fragment,
              try string_lit,
              num_lit
            ]
        )
  where
    template_fragment :: Parser s m (Literal s)
    template_fragment = TemplateFragment <$> (choice [try head_temp_frag, try mid_temp_frag, try tail_temp_frag, no_sub_temp_frag])
    no_sub_temp_frag = do
      char '`'
      contents <- many template_char
      char '`'
      pure $ NoSub $ fromText $ mconcat $ map toText $ contents
    head_temp_frag :: Parser s m (TemplateFragment s)
    head_temp_frag = do
      char '`'
      contents <- many template_char
      string "${"
      pure $ TemplateHead $ fromText $ mconcat $ map toText $ contents
    mid_temp_frag = do
      char '}'
      contents <- many template_char
      string "${"
      pure $ TemplateMiddle $ fromText $ mconcat $ map toText $ contents
    tail_temp_frag = do
      char '}'
      contents <- many template_char
      char '`'
      pure $ TemplateTail $ fromText $ mconcat $ map toText $ contents
    template_char :: Parser s m s
    template_char = fromText . toText <$> choice [try (string "$" <* (notFollowedBy $ char '{')), try (char '\\' *> ((try template_escape_seq) <|> not_escape_seq)), try ((optional $ char '\\') *> (eol)), source_char]
    source_char = error "TODO"
    template_escape_seq = error "TODO: TemplateEscapeSequence, prepend backslash"
    not_escape_seq = error "TODO: NotEscapeSequence, prepend backslash"
    string_lit = String <$> error "TODO"
    num_lit = Number <$> (choice [try decimal_literal, try decimal_bigint, try plain_bigint, try normal_integer, octal_int])
    decimal_literal = error "TODO"
    decimal_bigint = error "TODO"
    plain_bigint = error "TODO"
    normal_integer = error "TODO"
    octal_int = error "TODO"

fslash_handler :: (Logger m, Characters s) => Parser s m (Token s)
fslash_handler = error "TODO: Regex literal, division and division assignment"

punctuator :: (Logger m, Characters s) => Parser s m (Token s)
punctuator =
  Punc
    <$> ( choice
            [ try $ string ">>>=" *> pure UnsignedRightShiftAssign,
              try $ string "..." *> pure Spread,
              try $ string "===" *> pure TripleEqual,
              try $ string "!==" *> pure DoubleNotEqual,
              try $ string "<<=" *> pure LeftShiftAssign,
              try $ string ">>=" *> pure RightShiftAssign,
              try $ string ">>>" *> pure UnsignedRightShift,
              try $ string "**=" *> pure ExpAssign,
              try $ string "&&=" *> pure LogicalAndAssign,
              try $ string "||=" *> pure LogicalOrAssign,
              try $ string "??=" *> pure NullishAssign,
              try $ string "?." *> (notFollowedBy digitChar) *> pure OptionalChain,
              try $ string "**" *> pure Exp,
              try $ string "++" *> pure Inc,
              try $ string "--" *> pure Dec,
              try $ string "<=" *> pure LTEQ,
              try $ string ">=" *> pure GTEQ,
              try $ string "==" *> pure DoubleEqual,
              try $ string "!=" *> pure NotEqual,
              try $ string "<<" *> pure LeftShift,
              try $ string ">>" *> pure RightShift,
              try $ string "+=" *> pure AddAssign,
              try $ string "-=" *> pure SubAssign,
              try $ string "*=" *> pure MultAssign,
              try $ string "%=" *> pure ModAssign,
              try $ string "&=" *> pure BitwiseAndAssign,
              try $ string "|=" *> pure BitwiseOrAssign,
              try $ string "^=" *> pure BitwiseXorAssign,
              try $ string "&&" *> pure LogicalAnd,
              try $ string "||" *> pure LogicalOr,
              try $ string "??" *> pure Nullish,
              char '+' *> pure Add,
              char '-' *> pure Sub,
              char '*' *> pure Mult,
              char '%' *> pure Mod,
              char '<' *> pure Utilities.Javascript.LT,
              char '>' *> pure Utilities.Javascript.GT,
              char '&' *> pure BitwiseAnd,
              char '|' *> pure BitwiseOr,
              char '^' *> pure BitwiseXor,
              char '~' *> pure BitwiseNot,
              char '=' *> pure Assign,
              char '(' *> pure LParen,
              char ')' *> pure RParen,
              char '{' *> pure LCurly,
              char '}' *> pure RCurly,
              char '[' *> pure LSquare,
              char ']' *> pure RSquare,
              char '.' *> pure Dot,
              char ';' *> pure Semicolon,
              char ',' *> pure Comma,
              char '!' *> pure LogicalNot
            ]
        )

linebreak :: (Logger m, Characters s) => Parser s m (Token s)
linebreak = newline *> pure LineTerminator

whitespace :: (Logger m, Characters s) => Parser s m (Token s)
whitespace = hspace *> pure WhiteSpace
