module Restruct where

-- https://docutils.sourceforge.io/rst.html
-- https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html

-- https://hackage.haskell.org/package/parsec-3.1.18.0/docs/doc-index-All.html

--import Data.Text (Text)
--import Data.Void (Void)
--import Text.Parsec as P
--
--data RestElement
--  = RBody RestBody
--  | RTransition
--  | -- list of integers is the location in the section heirachy it is, Text is the title
--    -- NOTE: future me don't bother with proper restext convention do header depth via #n prefix to the title
--    RSection [Int] Text RestBody
--
--data RestBody
--  = RParagraph [RInlineText]
--  | RBulletList Void
--  | REnumList Void
--  | RDefinitionList Void
--  | RFieldList Void
--  | ROptionList Void
--  | RLiteralBlock Void
--  | RLineBlock Void
--  | RBlockQuote Void
--  | -- skipping doctest blocks because no I'll just use a literal block thanks
--    RTable Void
--  | RExplicit Void
--
--data MarkupModifier = Underline | Bold | Italic
--
--data RInlineText = RInLineText {text :: Text, modifiers :: [MarkupModifier]}
