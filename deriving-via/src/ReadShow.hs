{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ReadShow (ReadShow (..)) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Database.Persist.Sql (PersistField (..), PersistFieldSql (..), PersistValue (..))
import Text.Read (readEither, readMaybe)

newtype ReadShow a = ReadShow {unReadShow :: a}

instance (Show a) => ToJSON (ReadShow a) where
  toJSON = String . Text.pack . show . unReadShow

instance (Read a, Show a) => FromJSON (ReadShow a) where
  parseJSON (String value) =
    ReadShow <$> case readMaybe (Text.unpack value) of
      Just valid -> pure valid
      Nothing -> fail "Could not parse."
  parseJSON other = typeMismatch "String" other

instance (Read a, Show a) => PersistFieldSql (ReadShow a) where
  sqlType _ = sqlType $ Proxy @Text

instance (Read a, Show a) => PersistField (ReadShow a) where
  toPersistValue = toPersistValue . Text.pack . show . unReadShow
  fromPersistValue (PersistText x) = ReadShow <$> mapLeft Text.pack (readEither (Text.unpack x))
  fromPersistValue other = Left $ "Expected text, got " <> Text.pack (show other)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = \case
  Right val -> Right val
  Left err -> Left (f err)
