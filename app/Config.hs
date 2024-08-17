module Config where

outputDir :: String
outputDir = "publish"

assetGlobs :: [String]
assetGlobs = ["static//*"]

-- CAN ONLY BE TYPST DOCS UNLESS YOU CHANGE THINGS AT THE `pages` RULE in `Main.hs
pagePaths :: [String]
pagePaths = ["links.typ"]

postGlobs :: [String]
postGlobs = ["posts/*.typ", "posts/*.md"]
