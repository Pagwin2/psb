module Utilities where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

parseDate :: Text -> Maybe Text
parseDate str = do
  date <- parseTimeM False defaultTimeLocale "%Y-%-m-%-d" $ T.unpack str
  -- need to append the time to avoid potential issues
  pure $ T.pack $ formatTime @UTCTime defaultTimeLocale "%Y-%m-%dT00:00:00Z" date

tee :: (Monad m) => (a -> m b) -> m a -> m a
tee f v = v >>= (\underlying -> f underlying *> v)
