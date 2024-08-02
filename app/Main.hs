-- pulling heavily from https://abhinavsarkar.net/posts/static-site-generator-using-shake/
-- docs:
-- https://hackage.haskell.org/package/shake-0.19.8/docs/doc-index-All.html
-- https://hackage.haskell.org/package/pandoc-3.2.1/docs/doc-index-All.html
-- https://hackage.haskell.org/package/mustache-2.4.2/docs/doc-index.html
--

module Main where

import Control.Monad (forM)
import Data.List (sortOn)
import Development.Shake (Action, Rules, (|%>), (~>), (%>))
import Development.Shake.FilePath ((</>))
import qualified Data.HashMap.Strict as HM
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as Shake
import Config
import Types
import Utilities
import Templates
-- target = thing we want
-- Rule = pattern of thing being made + actions to produce the thing
-- Action = actions to produce a thing

main :: IO ()
main = Shake.shakeArgs Shake.shakeOptions $ do
  Shake.withTargetDocs "Build the site" $
    "build" ~> buildSite
  Shake.withTargetDocs "Clean the built site" $
    "clean" ~> Shake.removeFilesAfter outputDir ["//*"]
  Shake.withoutTargets buildRules

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

buildRules :: Rules ()
buildRules = do
  home
  assets
  pages
  postsRule
  rss

-- make a rule of the pattern outputDir/asset_name which copes from outputDir/../pages
assets :: Rules ()
assets = map (outputDir </>) assetGlobs |%> \target -> do
  let src = Shake.dropDirectory1 target
  Shake.copyFileChanged src target
  Shake.putInfo $ "Copied " <> target <> " from " <> src


pages :: Rules ()
pages = map indexHtmlOutputPath pagePaths |%> \target -> do
  let src = indexHtmlSourcePath target
  let metaSrc = indexHtmlMetaPath target
  html <- typstToHtml src
  meta <- yamlToPost metaSrc
  let page = Page (postTitle meta) html
  applyTemplateAndWrite "default.html" page target
  Shake.putInfo $ "Built " <> target <> " from " <> src

postsRule :: Rules ()
postsRule = map indexHtmlOutputPath postGlobs |%> \target -> do
  let src = indexHtmlSourcePath target
  post <- readPost src
  postHtml <- applyTemplate "post.html" post

  let page = Page (postTitle post) postHtml
  applyTemplateAndWrite "default.html" page target
  Shake.putInfo $ "Built " <> target <> " from " <> src

home :: Rules ()
home = outputDir </> "index.html" %> \target -> do
  postPaths <- Shake.getDirectoryFiles "" postGlobs
  posts <- take 3
    . sortOn (Ord.Down . postDate)
    <$> forM postPaths readPost
  html <- applyTemplate "home.html" $ HM.singleton "posts" posts

  let page = Page (T.pack "Home") html
  applyTemplateAndWrite "default.html" page target
  Shake.putInfo $ "Built " <> target

rss :: Rules ()
rss = outputDir </> "index.xml" %> \target -> do
    postPaths <- Shake.getDirectoryFiles "" postGlobs
    posts <- sortOn (Ord.Down . postDate) <$> forM postPaths readPost
    applyTemplateAndWrite "feed.xml" (HM.singleton "posts" posts) target
    
    Shake.putInfo $ "Built " <> target

readPost :: FilePath -> Action Post
readPost postPath = do
  html <- typstToHtml postPath
  post <- yamlToPost $ typstMetaPath postPath
  Shake.putInfo $ "Read " <> postPath
  return $ post
    { 
      postContent = Just html,
      postLink = Just . T.pack $ "/" <> Shake.dropExtension postPath <> "/"
    }

