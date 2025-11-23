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
    checkParallel $ Group "Parse Tests" [("all_compile", all_compiles)]
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
