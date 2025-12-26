module Utilities.Javascript
  ( minify,
  )
where

import Control.Applicative (Alternative (many), optional)
import Data.Maybe (maybeToList)
import Logger
import Text.Megaparsec (MonadParsec (try), choice)
import Utilities.Parsing

minify :: String -> String
minify = id

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
  | LParen
  | RParen
  | LCurly
  | RCurly
  | LSquare
  | RSquare
  | Dot
  | Spread
  | Semicolon
  | Comma
  | OptionalChain
  | Operator Operator

data Reserved = Await | Break | Case | Catch | Class | Const | Continue | Debugger | Default | Delete | Do | Else | Enum | Export | Extends | FalseVal | Finally | For | Function | If | Import | In | Instanceof | New | Null | Return | Super | Switch | This | Throw | TrueVal | Try | Typeof | Var | Void | While | With | Yield

data Literal s = Number s | String s | Regex s | TemplateFragment (TemplateFragment s)

data TemplateFragment s = NoSub s | TemplateHead s | TemplateMiddle s | TemplateTail s

data Operator = Add | Sub | Mult | Div | Mod | Exp | Inc | Dec | LT | GT | LTEQ | GTEQ | DoubleEqual | NotEqual | TripleEqual | DoubleNotEqual | LeftShift | RightShift {- >>> -} | UnsignedRightShift | BitwiseAnd | BitwiseOr | BitwiseXor | BitwiseNot | LogicalAnd | LogicalOr | LogicalNot {- ?? -} | Nullish | Assign | AddAssign | SubAssign | MultAssign | DivAssign | ModAssign | ExpAssign | LeftShiftAssign | RightShiftAssign | UnsignedRightShiftAssign | BitwiseAndAssign | BitwiseOrAssign | BitwiseXorAssign | LogicalAndAssign | LogicalOrAssign | NullishAssign

tokens :: (Logger m, Characters s) => Parser s m [Token s]
tokens = do
  hashbang <- (optional hashbang_comment)
  tokens <- many token
  pure $ (maybeToList hashbang) ++ tokens

token :: (Logger m, Characters s) => Parser s m (Token s)
token =
  choice
    [ try comment,
      try operator,
      try reserved_word,
      try identifier,
      try private_identifier,
      try literal,
      try punctuator
    ]

hashbang_comment :: (Logger m, Characters s) => Parser s m (Token s)
hashbang_comment = error "TODO"

comment :: (Logger m, Characters s) => Parser s m (Token s)
comment = error "TODO"

operator :: (Logger m, Characters s) => Parser s m (Token s)
operator = error "TODO"

reserved_word :: (Logger m, Characters s) => Parser s m (Token s)
reserved_word = error "TODO"

identifier :: (Logger m, Characters s) => Parser s m (Token s)
identifier = error "TODO"

private_identifier :: (Logger m, Characters s) => Parser s m (Token s)
private_identifier = error "TODO"

literal :: (Logger m, Characters s) => Parser s m (Token s)
literal = error "TODO"

punctuator :: (Logger m, Characters s) => Parser s m (Token s)
punctuator = error "TODO"
