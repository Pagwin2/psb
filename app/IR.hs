module IR where

import Data.Text

-- Html and Math tags come with their data because they are leaves for us
-- We aren't parsing that if we can avoid it
data Tag = Heading {level :: Int} | Paragraph | Blockquote | Code | Html {html :: Text} | Anchor | Italic | Bold | Math {mathML :: Text}

data Data = Ast {ast :: AST} | Text {text :: Text}

data AST = AST {tag :: Tag, child :: [Data]}

-- for processing math
-- https://hackage.haskell.org/package/typst-0.6.1/docs/Typst-Parse.html#v:parseTypst
-- and
-- https://hackage.haskell.org/package/typst-symbols-0.1.7/docs/Typst-Symbols.html
-- are going to be used for handling typst and
-- texmath for latex handling
