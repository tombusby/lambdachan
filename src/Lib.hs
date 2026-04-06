-- | Public re-exports for lambdachan.
module Lib
  ( runApp
  , defaultConfig
  , AppConfig (..)
  , DatabaseBackend (..)
  ) where

import LambdaChan.App (runApp)
import LambdaChan.Config (AppConfig (..), DatabaseBackend (..), defaultConfig)
