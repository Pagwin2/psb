{-# LANGUAGE OverloadedStrings #-}

module Logger (Logger (logError, logWarning, logInfo, logDebug)) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, modify)
import Control.Monad.Trans.Writer (WriterT, tell)
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import qualified Data.Text.IO as T

class (Monad m) => Logger m where
  logError :: T.Text -> m ()
  logWarning :: T.Text -> m ()
  logInfo :: T.Text -> m ()
  logDebug :: T.Text -> m ()

logIO :: T.Text -> T.Text -> IO ()
logIO kind msg = T.putStrLn $ kind <> ": " <> msg

instance Logger IO where
  logError = logIO "error"
  logWarning = logIO "warning"
  logInfo = logIO "info"
  logDebug = logIO "debug"

logState :: (Monad m) => T.Text -> StateT T.Text m ()
logState msg = modify (<> msg <> "\n")

instance {-# OVERLAPPING #-} (Monad m) => Logger (StateT T.Text m) where
  logError = logState
  logWarning = logState
  logInfo = logState
  logDebug = logState

logStateStr :: (Monad m) => T.Text -> StateT String m ()
logStateStr msg = modify (<> T.unpack msg <> "\n")

instance {-# OVERLAPPING #-} (Monad m) => Logger (StateT String m) where
  logError = logStateStr
  logWarning = logStateStr
  logInfo = logStateStr
  logDebug = logStateStr

instance {-# OVERLAPPING #-} (Monad m) => Logger (WriterT T.Text m) where
  logError = tell . (<> "\n")
  logWarning = tell . (<> "\n")
  logInfo = tell . (<> "\n")
  logDebug = tell . (<> "\n")

instance {-# OVERLAPPING #-} (Monad m) => Logger (WriterT String m) where
  logError = tell . T.unpack . (<> "\n")
  logWarning = tell . T.unpack . (<> "\n")
  logInfo = tell . T.unpack . (<> "\n")
  logDebug = tell . T.unpack . (<> "\n")

instance Logger Identity where
  logError = const $ pure ()
  logWarning = const $ pure ()
  logInfo = const $ pure ()
  logDebug = const $ pure ()

-- this isn't strictly correct but it's only used for ParsecT so it is practically correct
instance {-# OVERLAPPABLE #-} (MonadTrans mt, Logger m) => Logger (mt m) where
  logError = lift . logError
  logWarning = lift . logWarning
  logInfo = lift . logInfo
  logDebug = lift . logDebug
