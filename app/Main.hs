-- pulling heavily from https://abhinavsarkar.net/posts/static-site-generator-using-shake/
-- docs:
-- https://hackage.haskell.org/package/pandoc-3.2.1/docs/doc-index-All.html
-- https://hackage.haskell.org/package/mustache-2.4.2/docs/doc-index.html
--
{-# LANGUAGE ApplicativeDo, DataKinds, DeriveGeneric #-}
{-# LANGUAGE DerivingVia, LambdaCase, TypeApplications #-}

module Main where

import Control.Monad (forM, void)
import Data.Aeson.Types (Result (..))
import Data.List (nub, sortOn)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Deriving.Aeson
import Deriving.Aeson.Stock (PrefixedSnake)
import Development.Shake (Action, Rules, (%>), (|%>), (~>))
import Development.Shake.FilePath ((<.>), (</>))
import Text.Pandoc (Block (Plain), Meta (..), MetaValue (..), Pandoc (..))
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as Shake
import qualified Text.Mustache as Mus
import qualified Text.Mustache.Compile as Mus
import qualified Text.Pandoc as Pandoc

-- target = thing we want
-- Rule = pattern of thing being made + actions to produce the thing
-- Action = actions to produce a thing

main :: IO ()
main = Shake.shakeArgs Shake.shakeOptions $ do
  Shake.withTargetDocs "Build the site" $
    "build" ~> buildSite
  Shake.withTargetDocs "Clean the built site" $
    "clean" ~> Shake.removeFilesAfter outputDir ["//*"]

outputDir :: String
outputDir = "publish"

buildSite :: Action ()
buildSite = do
    -- static files
    assetPaths <- Shake.getDirectoryFiles "" assetGlobs
    --  path concat each asset path so it's output into the outputDir
    Shake.need $ map (outputDir </>) assetPaths
    
    -- take the misc pages which aren't blog posts and make their html files
    Shake.need $ map indexHtmlOutputPath pagePaths

    -- handle posts
    postPaths <- Shake.getDirectoryFiles "" postGlobs
    Shake.need $ map indexHtmlOutputPath postPaths

    -- remaining pages, index.xml = rss feed
    Shake.need $ map (outputDir </>) ["index.html", "index.xml"]

-- make a rule of the pattern outputDir/asset_name which copes from outputDir/../pages
assets :: Rules ()
assets = map (outputDir </>) assetGlobs |%> \target -> do
  let src = Shake.dropDirectory1 target </> "pages"
  Shake.copyFileChanged src target
  Shake.putInfo $ "Copied " <> target <> " from " <> src

typstToHtml :: FilePath -> Action Text
typstToHtml filePath = do
  content <- Shake.readFile' filePath
  Shake.quietly . Shake.traced "Typst to HTML" $ do
    doc <- runPandoc . Pandoc.readTypst readerOptions . T.pack $ content
    
    runPandoc . Pandoc.writeHtml5String writerOptions $ doc
  where
    readerOptions =
      Pandoc.def {Pandoc.readerExtensions = Pandoc.pandocExtensions}
    writerOptions =
      Pandoc.def {Pandoc.writerExtensions = Pandoc.pandocExtensions}

data Page = Page {pageTitle :: Text, pageContent :: Text}
  deriving (Show, Generic)
  deriving (ToJSON) via PrefixedSnake "page" Page

assetGlobs :: [String]
assetGlobs = ["static/*"]

pagePaths :: [String]
pagePaths = ["about.md", "contact.md"]

postGlobs :: [String]
postGlobs = ["posts/*.typ"]

runPandoc action =
      Pandoc.runIO (Pandoc.setVerbosity Pandoc.ERROR >> action)
        >>= either (fail . show) return

indexHtmlOutputPath :: FilePath -> FilePath
indexHtmlOutputPath srcPath =
  outputDir </> Shake.dropExtension srcPath </> "index.html"
