module Config where

outputDir :: String
outputDir = "publish"

assetGlobs :: [String]
assetGlobs = ["static//*", "robots.txt", "sw.js"]

-- CAN ONLY BE TYPST DOCS UNLESS YOU CHANGE THINGS AT THE `pages` RULE in `Main.hs
pagePaths :: [String]
pagePaths = []

postGlobs :: [String]
postGlobs = ["posts/*.md"]
