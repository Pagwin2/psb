module Config where

outputDir :: String
outputDir = "publish"

assetGlobs :: [String]
assetGlobs = ["static//*", "robots.txt", "favicon.ico"]

-- this insanity is to avoid repetition
resourceGlobs :: [String]
resourceGlobs = jsGlobs ++ cssGlobs

prependResources :: (Functor m) => m String -> m String
prependResources = fmap ("resources/" ++)

jsGlobs :: [String]
jsGlobs = prependResources $ liftA2 (++) ["js//*."] ["js", "mjs"]

cssGlobs :: [String]
cssGlobs = prependResources $ liftA2 (++) ["css//*."] ["css"]

-- CAN ONLY BE TYPST DOCS UNLESS YOU CHANGE THINGS AT THE `pages` RULE in `Main.hs
pagePaths :: [String]
pagePaths = []

postGlobs :: [String]
postGlobs = ["posts/*.md"]
