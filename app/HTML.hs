{-# LANGUAGE OverloadedStrings #-}

module HTML (compileToHTML) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import IR

tshow :: (Show s) => s -> T.Text
tshow = T.pack . show

compileToHTML :: Document -> T.Text
compileToHTML (Doc elements) = T.concat $ map elementToHTML elements

elementToHTML :: Element -> T.Text
elementToHTML (Heading (H {level, text})) = T.concat ["<h", tshow level, ">", serializeInlineToHTML text, "</h", tshow level, ">"]
--
elementToHTML (Code (C {language = m_language, code})) = T.concat ["<pre class=\"sourceCode ", language, "\"><code class=\"sourceCode ", language, "\">"]
  where
    language = fromMaybe "" m_language
elementToHTML (BlockQuote (Q elems)) = T.concat ["<blockquote>", serializeInlineToHTML elems, "</blockquote>"]
elementToHTML (List (L {list_type = Ordered, items})) = T.concat ["<ol>", generateLiElems items, "</ol>"]
elementToHTML (List (L {list_type = Unordered, items})) = T.concat ["<ul>", generateLiElems items, "</ul>"]
elementToHTML (HTML (HTMLTag {tagName, attributes, html_content})) = T.concat ["<", tagName, T.concat $ map (\(name, value) -> T.concat [name, "=", "\"", fromMaybe "" value, "\""]) attributes, ">", html_content, "</", tagName, ">"]
elementToHTML (Paragraph (P snippets)) = serializeInlineToHTML snippets
elementToHTML HorizontalRule = "<hr>"

generateLiElems :: [ListItem] -> T.Text
generateLiElems [] = ""
generateLiElems (LI {content, children} : remainder) =
  T.concat
    [ "<li>",
      -- We assume child lists are stricly after our contents
      -- if they aren't this is fucked
      serializeInlineToHTML content,
      T.concat $ map (elementToHTML . List) children,
      "</li>"
    ]

serializeInlineToHTML :: [InlineText] -> T.Text
serializeInlineToHTML [] = ""
serializeInlineToHTML (Text t : rem) = t <> serializeInlineToHTML rem
serializeInlineToHTML (Bold elems : rem) = T.concat ["<b>", serializeInlineToHTML elems, "</b>", serializeInlineToHTML rem]
serializeInlineToHTML (Italic elems : rem) = T.concat ["<i>", serializeInlineToHTML elems, "</i>", serializeInlineToHTML rem]
