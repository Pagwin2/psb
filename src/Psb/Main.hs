-- pulling heavily from https://abhinavsarkar.net/posts/static-site-generator-using-shake/
-- docs:
-- https://hackage.haskell.org/package/shake-0.19.8/docs/doc-index-All.html
-- https://hackage.haskell.org/package/pandoc-3.2.1/docs/doc-index-All.html
-- https://hackage.haskell.org/package/mustache-2.4.2/docs/doc-index.html
--

module Psb.Main where

import Config
import Control.Monad (when)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Ord as Ord
import qualified Data.Text as T
import Data.Traversable (traverse)
import Deriving.Aeson
import Deriving.Aeson.Stock (Vanilla)
import Development.Shake (Action, Rules, (%>), (|%>), (~>))
import qualified Development.Shake as Shake
import Development.Shake.FilePath ((</>))
import qualified Development.Shake.FilePath as FP
import Templates
import Types
import Utilities.Action (getPublishedPosts, isDraft', markdownToHtml, markdownToPost, now, psbProgress)
import qualified Utilities.CSS as CSS
import Utilities.FilePath (indexHtmlOutputPath, indexHtmlSourcePaths, isMarkdownPost, urlConvert)
import qualified Utilities.Javascript as JS

-- target = thing we want
-- Rule = pattern of thing being made + actions to produce the thing
-- Action = actions to produce a thing

-- note: live watch should be done outside of shake with the watcher then running shake which is rather annoying
main :: IO ()
main = do
  Shake.shakeArgs
    Shake.shakeOptions
      { Shake.shakeProgress = psbProgress,
        Shake.shakeColor = True
      }
    $ do
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

  -- handle js, css and anything else we want to process before moving
  Shake.need <$> Shake.getDirectoryFiles "" resourceGlobs

  -- take the misc pages which aren't blog posts and make their html files
  Shake.need $ map indexHtmlOutputPath pagePaths

  -- handle posts
  postPaths <- getPublishedPosts isDraft
  Shake.need $ map indexHtmlOutputPath postPaths

  -- remaining pages, index.xml = rss feed
  Shake.need $ map (outputDir </>) ["index.html", "index.xml"]

buildRules :: Rules ()
buildRules = do
  home
  assets
  postsRule
  rss
  -- TODO: add rules for specifically the checksummed files which depend on the non-checksummed files
  css_resources
  js_resources

-- make a rule of the pattern outputDir/asset_name which copes from outputDir/../pages
assets :: Rules ()
assets =
  map (outputDir </>) assetGlobs |%> \target -> do
    let src = FP.dropDirectory1 target
    Shake.copyFileChanged src target

css_resources :: Rules ()
css_resources =
  map (outputDir </>) cssGlobs |%> \target -> do
    src <- Shake.readFile' $ FP.dropDirectory1 target
    Shake.writeFileChanged target $ CSS.minify src

js_resources :: Rules ()
js_resources =
  map (outputDir </>) jsGlobs |%> \target -> do
    src <- Shake.readFile' $ FP.dropDirectory1 target
    Shake.writeFileChanged target $ JS.minify src

-- there's probably a better way of doing this that allows for the target's origin file extension to get passed in but for now we're doing brute force
postsRule :: Rules ()
postsRule =
  map indexHtmlOutputPath postGlobs |%> \target -> do
    let potentials = indexHtmlSourcePaths target
    Shake.forP
      potentials
      ( \path -> do
          exists <- Shake.doesFileExist path
          should <- if exists then not <$> isDraft path else pure False
          when
            should
            ( case FP.takeExtension path of
                ".md" -> markdownPost path
                _ -> error $ "invalid file extension for post " <> target
            )
      )
    return ()

markdownPost :: FP.FilePath -> Action ()
markdownPost src = do
  Shake.need [src]
  let target = indexHtmlOutputPath src

  post <- readMarkdownPost src
  let rPost = fromPost post
  postHtml <- applyTemplate "post.html" rPost

  time <- Utilities.Action.now
  -- Shake.putInfo $ T.unpack $ urlConvert target
  let page =
        Page
          { pageTitle = rPostTitle rPost,
            pageContent = postHtml,
            pageNow = time,
            pageUrl = urlConvert target
          }
  applyTemplateAndWrite "default.html" page target

-- Shake.putInfo $ "Built " <> target <> " from " <> src

home :: Rules ()
home =
  outputDir </> "index.html" %> \target -> do
    postPaths <- getPublishedPosts isDraft
    posts <-
      sortOn (Ord.Down . postDate)
        <$> traverse readPost postPaths
    let posts' = map fromPost posts
    html <- applyTemplate "home.html" $ HM.singleton "posts" posts'
    time <- Utilities.Action.now
    -- Shake.putInfo $ T.unpack $ urlConvert target
    let page =
          Page
            { pageTitle = T.pack "Home",
              pageContent = html,
              pageNow = time,
              pageUrl = urlConvert target
            }
    applyTemplateAndWrite "default.html" page target

-- Shake.putInfo $ "Built " <> target

data Rss = Rss
  { now :: T.Text,
    posts :: [RenderedPost]
  }
  deriving (Show, Generic)
  deriving (ToJSON) via Vanilla Rss

rss :: Rules ()
rss =
  outputDir </> "index.xml" %> \target -> do
    postPaths <- getPublishedPosts isDraft
    posts <- map fromPost . sortOn (Ord.Down . postDate) <$> traverse readPost postPaths
    time <- Utilities.Action.now
    applyTemplateAndWrite "feed.xml" (Rss time posts) target

-- Shake.putInfo $ "Built " <> target

readPost :: FilePath -> Action Post
readPost postPath = case FP.takeExtension postPath of
  ".md" -> readMarkdownPost postPath
  _ -> error $ "unknown file extension for file" <> postPath

readMarkdownPost :: FilePath -> Action Post
readMarkdownPost postPath = do
  (post, html) <- markdownToHtml postPath
  -- Shake.putInfo $ "Read " <> postPath
  return $
    post
      { postContent = Just html,
        postLink = Just . T.pack $ "/" <> FP.dropExtension postPath <> "/"
      }

postHandles :: [(FilePath -> Bool, FilePath -> Action Post)]
postHandles = [(isMarkdownPost, markdownToPost)]

isDraft :: FilePath -> Action Bool
isDraft = isDraft' postHandles
