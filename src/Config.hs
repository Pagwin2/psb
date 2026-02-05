module Config where

import Development.Shake.FilePath (addExtension, (</>))

outputDir :: FilePath
outputDir = "publish"

-- build artifacts go here
buildDir :: FilePath
buildDir = ".psb"

assetGlobs :: [String]
assetGlobs = ["static//*", "robots.txt", "favicon.ico"]

-- this used to be useful but does nothing now
resourceGlobs :: [String]
resourceGlobs = jsGlobs ++ cssGlobs

prependResources :: (Functor m) => m String -> m String
prependResources = fmap ("resources/" ++)

resourceHashPath :: FilePath -> FilePath
resourceHashPath input = outputDir </> addExtension input ".hash"

jsGlobs :: [String]
jsGlobs = prependResources $ ["js/entry.mjs"] -- prependResources $ liftA2 (++) ["js//*."] ["js", "mjs"]

cssGlobs :: [String]
cssGlobs = prependResources $ ["css/entry.css"] -- prependResources $ liftA2 (++) ["css//*."] ["css"]

-- CAN ONLY BE TYPST DOCS UNLESS YOU CHANGE THINGS AT THE `pages` RULE in `Main.hs
pagePaths :: [String]
pagePaths = []

postGlobs :: [String]
postGlobs = ["posts/*.md"]
