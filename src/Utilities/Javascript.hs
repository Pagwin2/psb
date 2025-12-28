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
import Data.Maybe (maybeToList)
import Data.String (IsString (fromString))
import Data.Void (Void)
import Logger
import Text.Megaparsec (MonadParsec (notFollowedBy, try), ParseErrorBundle, Stream (tokensToChunk), anySingle, choice, parse)
import Text.Megaparsec.Char (hspace, newline, string)
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

toTokens :: (Characters s) => s -> Either (ParseErrorBundle s Void) [Token s]
toTokens = parse tokens ""

displayToken :: (ToText s) => Token s -> s
displayToken _ = error "TODO"

-- yeah I guess I'm making a javascript tokenizer
-- s is either Text or String
-- Regex will be tokenized
data Token s
  = WhiteSpace
  | LineTerminator
  | SingleLineComment
  | MultiLineComment
  | HashBangComment
  | Identifier s
  | PrivateIdentifier s
  | ReservedWord Reserved
  | Literal (Literal s)
  | Punc Punctuator

data Reserved = Await | Break | Case | Catch | Class | Const | Continue | Debugger | Default | Delete | Do | Else | Enum | Export | Extends | FalseVal | Finally | For | Function | If | Import | In | Instanceof | New | Null | Return | Super | Switch | This | Throw | TrueVal | Try | Typeof | Var | Void | While | With | Yield

data Literal s = Number s | String s | Regex s | TemplateFragment (TemplateFragment s)

data TemplateFragment s = NoSub s | TemplateHead s | TemplateMiddle s | TemplateTail s

data Punctuator = Add | Sub | Mult | Div | Mod | Exp | Inc | Dec | LT | GT | LTEQ | GTEQ | DoubleEqual | NotEqual | TripleEqual | DoubleNotEqual | LeftShift | RightShift {- >>> -} | UnsignedRightShift | BitwiseAnd | BitwiseOr | BitwiseXor | BitwiseNot | LogicalAnd | LogicalOr | LogicalNot {- ?? -} | Nullish | Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign | ExpAssign | LeftShiftAssign | RightShiftAssign | UnsignedRightShiftAssign | BitwiseAndAssign | BitwiseOrAssign | BitwiseXorAssign | LogicalAndAssign | LogicalOrAssign | NullishAssign | LParen | RParen | LCurly | RCurly | LSquare | RSquare | Dot | Spread | Semicolon | Comma | OptionalChain

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
  many ((notFollowedBy newline) *> anySingle)
  pure HashBangComment

comment :: (Logger m, Characters s) => Parser s m (Token s)
comment = (try singleline_com) <|> multiline_com
  where
    singleline_com = do
      string "//"
      many ((notFollowedBy newline) *> anySingle)
      pure SingleLineComment
    multiline_com = do
      string "/*"
      many ((notFollowedBy $ string "*/") *> anySingle)
      pure MultiLineComment

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

identifier :: (Logger m, Characters s) => Parser s m (Token s)
identifier = do
  first <- start_char
  rem <- many rem_char
  let tmp = toString $ tokensToChunk (Proxy :: Proxy s) rem
  pure $ Identifier $ fromString (first : tmp)
  where
    start_char :: Parser s m (Token s)
    start_char = error "TODO"
    rem_char :: Parser s m (Token s)
    rem_char = error "TODO"

private_identifier :: (Logger m, Characters s) => Parser s m (Token s)
private_identifier = error "TODO"

literal :: (Logger m, Characters s) => Parser s m (Token s)
literal = error "TODO"

punctuator :: (Logger m, Characters s) => Parser s m (Token s)
punctuator = error "TODO"

linebreak :: (Logger m, Characters s) => Parser s m (Token s)
linebreak = newline *> pure WhiteSpace

whitespace :: (Logger m, Characters s) => Parser s m (Token s)
whitespace = hspace *> pure WhiteSpace
