module Logger.Shake where

import qualified Data.Text as T
import Development.Shake (Action)
import qualified Development.Shake as Shake
import Logger

instance Logger Action where
  logError = Shake.putError . T.unpack
  logWarning = Shake.putWarn . T.unpack
  logInfo = Shake.putInfo . T.unpack
  logDebug = Shake.putLoud . T.unpack
