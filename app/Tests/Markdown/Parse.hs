{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Either (isRight)
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Debug.Trace (traceShow)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import IR
import Markdown
import qualified Markdown
import System.Exit (exitFailure, exitSuccess)
import Text.Megaparsec

main :: IO ()
main = do
  cond <-
    checkParallel $
      Group
        "Parse Tests"
        [ ("all_compile", all_compiles),
          ("header_and_paragraph", header_and_paragraph),
          ("paragraph_and_header_and_paragraph", paragraph_and_header_and_paragraph),
          ("bold_and_header_and_paragraph", bold_and_header_and_paragraph),
          ("code_block", code_block)
        ]
  if cond
    then exitSuccess
    else exitFailure

all_compiles :: Property
all_compiles = property $ do
  xs <- forAll $ Gen.text (Range.linear 0 10) Gen.ascii
  annotate $ T.unpack xs
  let parsed = parse (Markdown.document :: ParsecT Void Text Identity IR.Document) "test_input" xs
  case parsed of
    Right _ -> success
    Left e -> fail $ errorBundlePretty e

header_and_paragraph :: Property
header_and_paragraph = property $ do
  header_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  header_level <- forAll $ Gen.int (Range.linear 1 6)
  paragraph_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha

  let input = (T.pack $ take header_level $ repeat '#') <> header_text <> "\n\n" <> paragraph_text

  let parsed = parse (Markdown.document :: ParsecT Void Text Identity IR.Document) "test_input" input

  case parsed of
    Right (Doc [Heading (H {level = header_level, text = [Text (header_text)]}), Paragraph (P ([Text paragraph_text]))]) -> success
    Right tree -> fail $ "Incorrect syntax tree: " <> show tree
    Left e -> fail $ errorBundlePretty e

paragraph_and_header_and_paragraph :: Property
paragraph_and_header_and_paragraph = property $ do
  paragraph1_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  header_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  header_level <- forAll $ Gen.int (Range.linear 1 6)
  paragraph2_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha

  let input = paragraph1_text <> "\n\n" <> (T.pack $ take header_level $ repeat '#') <> header_text <> "\n\n" <> paragraph2_text

  let parsed = parse (Markdown.document :: ParsecT Void Text Identity IR.Document) "test_input" input

  case parsed of
    Right (Doc [Paragraph (P ([Text paragarph1_text])), Heading (H {level = header_level, text = [Text (header_text)]}), Paragraph (P ([Text paragraph2_text]))]) -> success
    Right tree -> fail $ "Incorrect syntax tree: " <> show tree
    Left e -> fail $ errorBundlePretty e

bold_and_header_and_paragraph :: Property
bold_and_header_and_paragraph = property $ do
  bold_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  header_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  header_level <- forAll $ Gen.int (Range.linear 1 6)
  paragraph_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha

  let input = "**" <> bold_text <> "**\n\n" <> (T.pack $ take header_level $ repeat '#') <> header_text <> "\n\n" <> paragraph_text

  let parsed = parse (Markdown.document :: ParsecT Void Text Identity IR.Document) "test_input" input

  case parsed of
    Right (Doc [Paragraph (P ([Bold [Text bold_text]])), Heading (H {level = header_level, text = [Text (header_text)]}), Paragraph (P ([Text paragraph_text]))]) -> success
    Right tree -> fail $ "Incorrect syntax tree: " <> show tree
    Left e -> fail $ errorBundlePretty e

code_block :: Property
code_block = property $ do
  language <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  code <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  let input = "```" <> language <> "\n" <> code <> "```"

  let parsed = parse (Markdown.document :: ParsecT Void Text Identity IR.Document) "test_input" input

  case parsed of
    Right (Doc [Code (C {language, code})]) -> success
    Right tree -> fail $ "Incorrect syntax tree: " <> show tree
    Left e -> fail $ errorBundlePretty e

-- from new writing learning
-- Error case, joins block quotes toegether
-- > [*Sic*](https://www.merriam-webster.com/dictionary/sic) usually appears in parentheses or brackets, sometimes with the letters in italics. In this context it means “intentionally so written.” On its own, *sic* means “so” or “thus” and can be found in phrases such as *sic transit gloria mundi* ("so passes away the glory of the world") and *sic semper tyrannis* ("thus ever to tyrants," the motto of the state of Virginia).
--
-- > What is denoted by *sic* is that the word or phrase that precedes it occurs in the original passage being quoted or name being used and was not introduced by the writer doing the quoting.
--

-- from micro blogs
-- Error case 2: made one bullet instead of two, happens for any number of items and probably applicable to numbered list as well
-- - item 1
-- - item 2

-- From weekly notes, doing a header after a unordered list causes it to be seens as an inline element
--
-- From all-projects, not sure if it's block or what causing HTML escapes to go haywire
