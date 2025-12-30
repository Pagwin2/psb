{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utilities.Javascript
  ( minify,
    toTokens,
    displayToken,
  )
where

import Control.Applicative (Alternative (many), optional, (<|>))
import Data.Data (Proxy (Proxy))
import Data.Functor ((<&>))
import Data.Maybe (maybeToList)
import Data.String (IsString (fromString))
import Data.Void (Void)
import Logger
import Text.Megaparsec (MonadParsec (notFollowedBy, try), ParseErrorBundle, Stream (tokensToChunk), anySingle, choice, parse)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, digitChar, hspace, letterChar, newline, string)
import Utilities.Parsing

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

toTokens :: (Characters s) => String -> s -> Either (ParseErrorBundle s Void) [Token s]
toTokens = parse tokens

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
displayLiteral (Regex s) = fromText $ "/" <> toText s <> "/"
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

data Literal s = Number s | String s | Regex s | TemplateFragment (TemplateFragment s) deriving (Eq)

data TemplateFragment s = NoSub s | TemplateHead s | TemplateMiddle s | TemplateTail s deriving (Eq)

data Punctuator = Add | Sub | Mult | Div | Mod | Exp | Inc | Dec | LT | GT | LTEQ | GTEQ | DoubleEqual | NotEqual | TripleEqual | DoubleNotEqual | LeftShift | RightShift {- >>> -} | UnsignedRightShift | BitwiseAnd | BitwiseOr | BitwiseXor | BitwiseNot | LogicalAnd | LogicalOr | LogicalNot {- ?? -} | Nullish | Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign | ExpAssign | LeftShiftAssign | RightShiftAssign | UnsignedRightShiftAssign | BitwiseAndAssign | BitwiseOrAssign | BitwiseXorAssign | LogicalAndAssign | LogicalOrAssign | NullishAssign | LParen | RParen | LCurly | RCurly | LSquare | RSquare | Dot | Spread | Semicolon | Comma | OptionalChain deriving (Eq)

tokens :: (Logger m, Characters s) => Parser s m [Token s]
tokens = do
  hashbang <- (optional hashbang_comment)
  tokens <- many token
  pure $ (maybeToList hashbang) ++ tokens

token :: (Logger m, Characters s) => Parser s m (Token s)
token =
  choice
    [ try comment,
      try reserved_word,
      try identifier,
      try private_identifier,
      try literal,
      try punctuator,
      try linebreak,
      whitespace
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
reserved_word = choice [try await, try break, try case_, try catch_, try class_, try const, try continue, try debugger, try default_, try delete, try do_, try else_, try enum, try export, try extends, try false, try finally_, try for_, try function, try if_, try import_, try in_, try instanceof, try new, try null, try return, try super, try switch, try this, try throw_, try true, try try_, try typeof, try var, try void, try while, try with, yield]
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

literal :: (Logger m, Characters s) => Parser s m (Token s)
literal =
  Literal
    <$> ( choice
            [ try template_fragment,
              try string_lit,
              num_lit
            ]
        )
  where
    template_fragment = TemplateFragment <$> error "TODO"
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
linebreak = newline *> pure WhiteSpace

whitespace :: (Logger m, Characters s) => Parser s m (Token s)
whitespace = hspace *> pure WhiteSpace
