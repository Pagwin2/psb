{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Utilities.Parsing where

import Data.String (IsString)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec (ParsecT, Stream, Token, Tokens)

type Parser = ParsecT Void

class (Token s ~ Char, Stream s, ToText (Tokens s), ToText s, IsString (Tokens s), IsString s, Monoid (Tokens s), ToChar (Token s), Eq (Tokens s), Show s) => Characters s

class ToText t where
  toText :: t -> Text
  fromText :: Text -> t
  toString :: t -> String

class ToChar c where
  toChar :: c -> Char
  fromChar :: Char -> c

instance ToChar Char where
  toChar = id
  fromChar = id

instance Characters Text

instance ToText Text where
  toText = id
  fromText = id
  toString = unpack

instance Characters String

instance ToText String where
  toText = pack
  fromText = unpack
  toString = id
