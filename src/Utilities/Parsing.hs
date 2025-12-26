{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Utilities.Parsing where

import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (ParsecT, Stream, Token, Tokens)

type Parser = ParsecT Void

class (Token s ~ Char, Stream s, ToText (Tokens s), IsString (Tokens s), Monoid (Tokens s), Eq (Tokens s), Show s) => Characters s

class ToText t where
  toText :: t -> Text

instance Characters Text

instance ToText Text where
  toText = id

instance Characters String

instance ToText String where
  toText = pack
