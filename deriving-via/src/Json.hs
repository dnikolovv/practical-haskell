{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Json
  ( JsonCamelCase (..),
    JsonSnakeCase (..),
  )
where

import Data.Aeson
import Data.Aeson.Casing
import GHC.Generics

newtype JsonCamelCase a = JsonCamelCase {unJsonCamelCase :: a}

instance (Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (JsonCamelCase a) where
  toJSON = genericToJSON defaultOptions . unJsonCamelCase

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (JsonCamelCase a) where
  parseJSON v = JsonCamelCase <$> genericParseJSON defaultOptions v

newtype JsonSnakeCase a = JsonSnakeCase {unJsonSnakeCase :: a}

instance (Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (JsonSnakeCase a) where
  toJSON = genericToJSON snakeCaseOptions . unJsonSnakeCase

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (JsonSnakeCase a) where
  parseJSON v = JsonSnakeCase <$> genericParseJSON snakeCaseOptions v

snakeCaseOptions :: Options
snakeCaseOptions = defaultOptions {fieldLabelModifier = snakeCase}