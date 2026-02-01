module Utilities.Bundling
  ( bundled,
  )
where

import Development.Shake (Action, Rules, command_)

-- does not include specification of output location or input files
generic_esbuild_options :: [String]
generic_esbuild_options = ["--minify", "--sourcemap", "--bundle", "--chunk-names=[name]-[hash]"]

-- TODO: not sure if I want all bundling to be an all at once afair, per file format
-- or multiple stages for various formats
--
-- Regardless the objective is to produce all of that while outputting a file to
-- indicate completion/fulfill a need directive without rebuilding even when files
-- are left unchanged, maybe have the need be a $(filename).hash which we compute
-- ourselves based on the unminified input
bundled :: Rules ()
bundled = error "TODO"
