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
elementToHTML (Code (C {language = m_language, code})) = T.concat ["<pre class=\"sourceCode ", language, "\"><code class=\"sourceCode ", language, "\">", code, "</code>", "</pre>"]
  where
    language = fromMaybe "" m_language
elementToHTML (BlockQuote (Q elems)) = T.concat ["<blockquote>", serializeInlineToHTML elems, "</blockquote>"]
elementToHTML (List (L {list_type = Ordered, items})) = T.concat ["<ol>", generateLiElems items, "</ol>"]
elementToHTML (List (L {list_type = Unordered, items})) = T.concat ["<ul>", generateLiElems items, "</ul>"]
elementToHTML (HTML (HTMLTag {html_content})) = html_content
elementToHTML (Paragraph (P snippets)) = T.concat ["<p>", serializeInlineToHTML snippets, "</p>"]
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
      "</li>",
      generateLiElems remainder
    ]

serializeInlineToHTML :: [InlineText] -> T.Text
serializeInlineToHTML [] = ""
serializeInlineToHTML (Text t : remaining) = t <> serializeInlineToHTML remaining
serializeInlineToHTML (Bold elems : remaining) = T.concat ["<b>", serializeInlineToHTML elems, "</b>", serializeInlineToHTML remaining]
serializeInlineToHTML (Italic elems : remaining) = T.concat ["<i>", serializeInlineToHTML elems, "</i>", serializeInlineToHTML remaining]
serializeInlineToHTML (InlineCode code : remaining) = T.concat ["<code>", code, "</code>", serializeInlineToHTML remaining]
serializeInlineToHTML (Link {linkText, url, title} : remaining) = T.concat ["<a href=\"", url, "\" ", maybe "" (\t -> T.concat ["title=\"", t, "\""]) title, "\">", serializeInlineToHTML linkText, "</a>", serializeInlineToHTML remaining]
serializeInlineToHTML (Image {altText, url, title} : remaining) = T.concat ["<img src=\">", url, "\" alt=\"", altText, "\"", maybe "" (\t -> T.concat ["title=\"", t, "\""]) title, ">", serializeInlineToHTML remaining]
serializeInlineToHTML (HTMLInline {inline_html_content} : remaining) = inline_html_content <> serializeInlineToHTML remaining
