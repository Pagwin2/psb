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
elementToHTML (Heading header) = T.concat ["<h", tshow header.level, ">", serializeInlineToHTML header.text, "</h", tshow header.level, ">"]
--
elementToHTML (Code code_block) = T.concat ["<pre class=\"sourceCode ", language, "\"><code class=\"sourceCode ", language, "\">", code_block.code, "</code>", "</pre>"]
  where
    language = fromMaybe "" code_block.language
elementToHTML (BlockQuote (Q elems)) = T.concat ["<blockquote>", serializeInlineToHTML elems, "</blockquote>"]
elementToHTML (List (L {list_type = Ordered, items})) = T.concat ["<ol>", generateLiElems items, "</ol>"]
elementToHTML (List (L {list_type = Unordered, items})) = T.concat ["<ul>", generateLiElems items, "</ul>"]
elementToHTML (HTML (HTMLTag {html_content})) = html_content
elementToHTML (Paragraph (P snippets)) = T.concat ["<p>", serializeInlineToHTML snippets, "</p>"]
elementToHTML HorizontalRule = "<hr>"

generateLiElems :: [ListItem] -> T.Text
generateLiElems [] = ""
generateLiElems (element : remainder) =
  T.concat
    [ "<li>",
      -- We assume child lists are stricly after our contents
      -- if they aren't this is fucked
      serializeInlineToHTML element.content,
      T.concat $ map (elementToHTML . List) element.children,
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
