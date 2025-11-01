{-# LANGUAGE OverloadedStrings #-}

module HTML (compileToHTML) where

import Data.Text
import IR

compileToHTML :: Document -> Text
compileToHTML = const ""
