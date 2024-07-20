module Config where

outputDir :: String
outputDir = "publish"

assetGlobs :: [String]
assetGlobs = ["static/*"]

pagePaths :: [String]
pagePaths = ["links.typ"]

postGlobs :: [String]
postGlobs = ["posts/*.typ"]
