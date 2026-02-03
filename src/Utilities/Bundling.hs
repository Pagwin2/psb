{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Utilities.Bundling
  ( bundled,
  )
where

import Config (buildDir, cssGlobs, jsGlobs, outputDir)
import Development.Shake (Action, RuleResult, Rules, addOracle, cmd_, command_, getDirectoryFiles, need, (%>))
import Development.Shake.Classes
import Development.Shake.FilePath ((</>))
import GHC.Generics (Generic)

-- does not include specification of output location or input files
generic_esbuild_options :: [String]
generic_esbuild_options = ["--minify", "--sourcemap", "--bundle", "--chunk-names=[name]-[hash]", "--entry-names=[name]-[hash]"]

data BuildOracleVariant = CSS | Javascript deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type BuildOutputs = [FilePath]

type instance RuleResult BuildOracleVariant = BuildOutputs

resource_dir :: FilePath
resource_dir = outputDir </> "resources"

-- TODO: not sure if I want all bundling to be an all at once afair, per file format
-- or multiple stages for various formats
--
-- Regardless the objective is to produce all of that while outputting a file to
-- indicate completion/fulfill a need directive without rebuilding even when files
-- are left unchanged, maybe have the need be a $(filename).hash which we compute
-- ourselves based on the unminified input
bundled :: Rules (BuildOracleVariant -> Action BuildOutputs)
bundled = addOracle $ \q -> case q of
  CSS -> bundle_css
  Javascript -> bundle_scripts

css_dir :: FilePath
css_dir = resource_dir </> "css"

css_meta_file :: FilePath
css_meta_file = buildDir </> "esbuild-css-meta.json"

css_esbuild_options :: [String]
css_esbuild_options =
  [ "--outdir=" ++ css_dir,
    "--loader:.png=file",
    "--loader:.woff2=file",
    "--loader:.svg=file",
    "--metafile=" ++ css_meta_file
  ]

-- need to take an input of resouces/blah.css
-- and in addition to bundling it
bundle_css :: Action BuildOutputs
bundle_css = do
  need cssGlobs
  css_files <- getDirectoryFiles "" cssGlobs
  cmd_ "esbuild" (generic_esbuild_options ++ css_esbuild_options ++ css_files)
  pure $ error "TODO: pull the list of files from the meta file"

-- Javascript and typescript
-- potentially:
-- "--target=es2020"
-- , "--format=esm"
js_esbuild_options :: [String]
js_esbuild_options = ["--outdir=" ++ js_dir, "--splitting", "--metafile=" ++ js_meta_file]

js_meta_file :: FilePath
js_meta_file = buildDir </> "esbuild-js-meta.json"

js_dir :: FilePath
js_dir = resource_dir </> "js"

bundle_scripts :: Action BuildOutputs
bundle_scripts = do
  need jsGlobs
  js_files <- getDirectoryFiles "" cssGlobs
  cmd_ "esbuild" (generic_esbuild_options ++ js_esbuild_options ++ js_files)
  pure $ error "TODO: pull the list of files from the meta file"
