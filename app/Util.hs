module Util where

import           Control.Monad.Catch (Exception)
import           Universum           (Show, Text)

data LolException = LolException Text
    deriving (Show)

instance Exception LolException

explicitEmpty :: Text -> Text
explicitEmpty "" = "<empty>"
explicitEmpty t  = t





