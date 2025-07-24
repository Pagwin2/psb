module IR where

import Data.Text

newtype Document = Doc [Element]

data Element = Heading Heading | Code Code | BlockQuote BlockQuote | List List | Table Table | HTML HTML | Paragraph Paragraph | BlankLine BlankLine

data Heading = H {level :: Int, text :: Text}

data Code = C {language :: Text, code :: Text}

newtype BlockQuote = Q Text

data ListType = Ordered | Unordered

data ListItem = LI {content :: Text, children :: [List]}

data List = L {list_type :: ListType, items :: [ListItem]}

data Table = T {header :: TableHeader, rows :: [TableRow]}

-- TODO: layout/sizing info?
newtype TableHeader = TH [Text]

newtype TableRow = TR Text

newtype HTML = Raw Text

newtype Paragraph = P [InlineText]

data InlineText = Normal Text | Bold InlineText | Italic InlineText | CodeLine Text | Link {nest :: InlineText, href :: Text}

data BlankLine = BL

-- for processing math
-- https://hackage.haskell.org/package/typst-0.6.1/docs/Typst-Parse.html#v:parseTypst
-- and
-- https://hackage.haskell.org/package/typst-symbols-0.1.7/docs/Typst-Symbols.html
-- are going to be used for handling typst and
-- texmath for latex handling
