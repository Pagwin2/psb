module Config where

outputDir :: String
outputDir = "publish"

assetGlobs :: [String]
assetGlobs = ["static/*"]

pagePaths :: [String]
pagePaths = ["about.md", "contact.md"]

postGlobs :: [String]
postGlobs = ["posts/*.typ"]
