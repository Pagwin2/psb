{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Either (isRight)
import Data.Functor.Identity (Identity (Identity))
import Data.String (IsString)
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
import System.Timeout (timeout)
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
          ("code_block", code_block),
          ("code_block_hanging", code_block_hanging),
          ("two_blockquotes", two_blockquotes),
          ("unordered_list", unordered_list),
          ("header_after_unordered_list", header_after_unordered_list),
          ("ordered_list", ordered_list),
          ("multiple_ordered_lists", multiple_ordered_lists),
          ("header_then_ordered_list", header_then_ordered_list),
          ("nested_unordered_list", nested_unordered_list)
          -- ("",),
        ]
  if cond
    then exitSuccess
    else exitFailure

-- timeout of 1 second, all of these tests should be completely clear of that, if they run longer they should fail
generic_parse inp = lift $ timeout 1000000 $ evaluate $ parse (Markdown.document :: ParsecT Void Text Identity IR.Document) "test_input" inp

all_compiles :: Property
all_compiles = property $ do
  xs <- forAll $ Gen.text (Range.linear 0 10) Gen.ascii
  annotate $ T.unpack xs
  parsed <- generic_parse xs
  case parsed of
    Nothing -> fail $ "Hit Timeout"
    (Just (Right _)) -> success
    (Just (Left e)) -> fail $ errorBundlePretty e

header_and_paragraph :: Property
header_and_paragraph = property $ do
  header_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  header_level <- forAll $ Gen.int (Range.linear 1 6)
  paragraph_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha

  let input = (T.pack $ take header_level $ repeat '#') <> header_text <> "\n\n" <> paragraph_text

  parsed <- generic_parse input

  case parsed of
    Nothing -> fail $ "Hit Timeout"
    (Just (Right (Doc [Heading (H {level = header_level, text = [Text (header_text)]}), Paragraph (P ([Text paragraph_text]))]))) -> success
    (Just (Right tree)) -> fail $ "Incorrect syntax tree: " <> show tree
    (Just (Left e)) -> fail $ errorBundlePretty e

paragraph_and_header_and_paragraph :: Property
paragraph_and_header_and_paragraph = property $ do
  paragraph1_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  header_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  header_level <- forAll $ Gen.int (Range.linear 1 6)
  paragraph2_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha

  let input = paragraph1_text <> "\n\n" <> (T.pack $ take header_level $ repeat '#') <> header_text <> "\n\n" <> paragraph2_text

  parsed <- generic_parse input

  case parsed of
    Nothing -> fail $ "Hit Timeout"
    (Just (Right (Doc [Paragraph (P ([Text paragarph1_text])), Heading (H {level = header_level, text = [Text (header_text)]}), Paragraph (P ([Text paragraph2_text]))]))) -> success
    (Just (Right tree)) -> fail $ "Incorrect syntax tree: " <> show tree
    (Just (Left e)) -> fail $ errorBundlePretty e

bold_and_header_and_paragraph :: Property
bold_and_header_and_paragraph = property $ do
  bold_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  header_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  header_level <- forAll $ Gen.int (Range.linear 1 6)
  paragraph_text <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha

  let input = "**" <> bold_text <> "**\n\n" <> (T.pack $ take header_level $ repeat '#') <> header_text <> "\n\n" <> paragraph_text

  parsed <- generic_parse input

  case parsed of
    Nothing -> fail $ "Hit Timeout"
    (Just (Right (Doc [Paragraph (P ([Bold [Text bold_text]])), Heading (H {level = header_level, text = [Text (header_text)]}), Paragraph (P ([Text paragraph_text]))]))) -> success
    (Just (Right tree)) -> fail $ "Incorrect syntax tree: " <> show tree
    (Just (Left e)) -> fail $ errorBundlePretty e

code_block :: Property
code_block = property $ do
  language <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  code <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  let input = "```" <> language <> "\n" <> code <> "\n```"
  annotate $ "Input: " <> T.unpack input
  parsed <- generic_parse input

  case parsed of
    Nothing -> fail $ "Hit Timeout"
    (Just (Right (Doc [Code (C {language, code})]))) -> success
    (Just (Right tree)) -> fail $ "Incorrect syntax tree: " <> show tree
    (Just (Left e)) -> fail $ errorBundlePretty e

code_block_hanging :: Property
code_block_hanging = property $ do
  language <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  code <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  let input = "```" <> language <> "\n" <> code <> "```"
  annotate $ "Input: " <> T.unpack input
  parsed <- generic_parse input

  case parsed of
    Nothing -> fail $ "Hit Timeout"
    -- we're just testing for hanging
    (Just (Right _)) -> success
    (Just (Left e)) -> fail $ errorBundlePretty e

two_blockquotes :: Property
two_blockquotes = property $ do
  let text_gen = forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  text_1 <- text_gen
  text_2 <- text_gen
  let input = "> " <> text_1 <> "\n\n> " <> text_2

  parsed <- generic_parse input

  case parsed of
    Nothing -> fail $ "Hit Timeout"
    (Just (Right (Doc [BlockQuote (Q [Text text_1]), BlockQuote (Q [Text text_2])]))) -> success
    (Just (Right tree)) -> fail $ "Incorrect syntax tree: " <> show tree
    (Just (Left e)) -> fail $ errorBundlePretty e

unordered_list :: Property
unordered_list = property $ do
  let text_gen = forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  text_1 <- text_gen
  text_2 <- text_gen
  let input = "- " <> text_1 <> "\n- " <> text_2

  parsed <- generic_parse input

  case parsed of
    Nothing -> fail $ "Hit Timeout"
    (Just (Right (Doc [List (L {list_type = Unordered, items = [LI {content = [Text text_1], children = []}, LI {content = [Text text_2], children = []}]})]))) -> success
    (Just (Right tree)) -> fail $ "Incorrect syntax tree: " <> show tree
    (Just (Left e)) -> fail $ errorBundlePretty e

header_after_unordered_list :: Property
header_after_unordered_list = property $ do
  let text_gen = forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  bullet_text <- text_gen
  header_text <- text_gen
  header_level <- forAll $ Gen.int (Range.linear 1 6)

  let input = "- " <> bullet_text <> "\n\n" <> (T.pack $ take header_level $ repeat '#') <> header_text

  parsed <- generic_parse input

  case parsed of
    Nothing -> fail $ "Hit Timeout"
    (Just (Right (Doc [List (L {list_type = Unordered, items = [LI {content = [Text bullet_text], children = []}]}), Heading (H {level = header_level, text = [Text header_text]})]))) -> success
    (Just (Right tree)) -> fail $ "Incorrect syntax tree: " <> show tree
    (Just (Left e)) -> fail $ errorBundlePretty e

ordered_list :: Property
ordered_list = property $ do
  let text_gen = forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  item_1 <- text_gen
  item_2 <- text_gen
  item_3 <- text_gen
  let input = "1. " <> item_1 <> "\n2. " <> item_2 <> "\n3. " <> item_3

  parsed <- generic_parse input

  case parsed of
    Nothing -> fail $ "Hit Timeout"
    (Just (Right (Doc [List (L {list_type = Ordered, items = [LI {content = [Text item_1], children = []}, LI {content = [Text item_2], children = []}, LI {content = [Text item_3], children = []}]})]))) -> success
    (Just (Right tree)) -> fail $ "Incorrect syntax tree: " <> show tree
    (Just (Left e)) -> fail $ errorBundlePretty e

multiple_ordered_lists :: Property
multiple_ordered_lists = property $ do
  let text_gen = forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  item_1 <- text_gen
  item_2 <- text_gen
  item_3 <- text_gen
  let input = "1. " <> item_1 <> "\n\n2. " <> item_2 <> "\n\n3. " <> item_3

  parsed <- generic_parse input
  case parsed of
    Nothing -> fail $ "Hit Timeout"
    ( Just
        ( Right
            ( Doc
                [ List (L {list_type = Ordered, items = [LI {content = [Text item_1], children = []}]}),
                  List (L {list_type = Ordered, items = [LI {content = [Text item_2], children = []}]}),
                  List (L {list_type = Ordered, items = [LI {content = [Text item_3], children = []}]})
                  ]
              )
          )
      ) -> success
    (Just (Right tree)) -> fail $ "Incorrect syntax tree: " <> show tree
    (Just (Left e)) -> fail $ errorBundlePretty e

-- - a
--      - a
-- - b
nested_unordered_list :: Property
nested_unordered_list = property $ do
  let text_gen = forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  item_1 <- text_gen
  item_2 <- text_gen
  item_3 <- text_gen
  let input = "- " <> item_1 <> "\n\n    -" <> item_2 <> "\n\n- " <> item_3

  parsed <- generic_parse input
  case parsed of
    Nothing -> fail $ "Hit Timeout"
    ( Just
        ( Right
            ( Doc
                [ List (L {list_type = Unordered, items = [LI {content = [Text item_1], children = [L {list_type = Unordered, items = [LI {content = [Text item_2]}]}]}, LI {content = [Text item_3], children = []}]})
                  ]
              )
          )
      ) -> success
    (Just (Right tree)) -> fail $ "Incorrect syntax tree: " <> show tree
    (Just (Left e)) -> fail $ errorBundlePretty e

-- ##
-- 1)
-- 2)
-- 3)
header_then_ordered_list :: Property
header_then_ordered_list = property $ do
  let text_gen = forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  header <- text_gen
  header_level <- forAll $ Gen.int (Range.linear 1 6)
  item_1 <- text_gen
  item_2 <- text_gen
  item_3 <- text_gen
  let input = (T.pack $ take header_level $ repeat '#') <> header <> "1) " <> item_1 <> "\n2) " <> item_2 <> "\n3) " <> item_3

  parsed <- generic_parse input

  case parsed of
    Nothing -> fail $ "Hit Timeout"
    ( Just
        ( Right
            ( Doc
                [ Heading (H {level = header_level, text = header}),
                  List
                    ( L
                        { list_type = Ordered,
                          items =
                            [ LI {content = [Text item_1], children = []},
                              LI {content = [Text item_2], children = []},
                              LI {content = [Text item_3], children = []}
                              ]
                        }
                      )
                  ]
              )
          )
      ) -> success
    (Just (Right tree)) -> fail $ "Incorrect syntax tree: " <> show tree
    (Just (Left e)) -> fail $ errorBundlePretty e
