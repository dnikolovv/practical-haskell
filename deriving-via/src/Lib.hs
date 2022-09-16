{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( demoExternalAPI,
    demoReadShow,
    demoJson,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
-- Note that you need to import the constructors as well

import Data.Aeson
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy.Char8 qualified as BS
import Database.Persist.Sql (PersistField (..), PersistFieldSql, PersistValue (..))
import ExternalAPI
  ( ExternalAPI (..),
    ExternalThing (..),
    MockedExternalAPI (..),
    RealExternalAPIClient (..),
  )
import GHC.Generics
import Json (JsonCamelCase (..), JsonSnakeCase (..))
import ReadShow

data DemoStatusType
  = Active
  | Inactive
  deriving (Read, Show)
  deriving
    ( ToJSON,
      FromJSON,
      PersistFieldSql,
      PersistField
    )
    via (ReadShow DemoStatusType)

-- ghci> demoReadShow
-- PersistText "Active"
-- Right Active
-- "Inactive"
-- Success Inactive

demoReadShow :: IO ()
demoReadShow = do
  print $ toPersistValue Active
  print $ fromPersistValue @DemoStatusType (PersistText "Active")

  BS.putStrLn $ encode Inactive
  print $ JSON.fromJSON @DemoStatusType (JSON.String "Inactive")

data SomeRecord = SomeRecord
  { aField :: Int,
    anotherField :: Int
  }
  deriving (Generic)
  deriving (ToJSON, FromJSON) via (JsonCamelCase SomeRecord)

data AnotherRecord = AnotherRecord
  { someField :: Int,
    someOtherField :: Int
  }
  deriving (Generic)
  deriving (ToJSON, FromJSON) via (JsonSnakeCase AnotherRecord)

-- ghci> demoJson
-- {"aField":123,"anotherField":123}
-- {"some_field":123,"some_other_field":123}
-- ghci>

demoJson :: IO ()
demoJson = do
  -- will output {"aField":123,"anotherField":123}
  BS.putStrLn . encode $ SomeRecord 123 123
  -- will output {"some_field":123,"some_other_field":123}
  BS.putStrLn . encode $ AnotherRecord 123 123

-- * Lib> demoExternalAPI

-- Calling real API with id 123...
-- Posting to the real API...
-- Called mocked getExternal with id 123...
-- Called mocked postExternal...

-- * Lib>

demoExternalAPI :: IO ()
demoExternalAPI = do
  -- We can substitute the ExternalAPI
  -- implementation by choosing a different monad
  runAppM externalAPIAction
  runTestAppM externalAPIAction
  where
    externalAPIAction ::
      Monad m =>
      ExternalAPI m =>
      m ()
    externalAPIAction = do
      void $ getExternal 123
      void $ postExternal ExternalThing

-- The real application monad that handles your business logic
newtype AppM a = AppM {runAppM :: IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO
    )
  deriving (ExternalAPI) via (RealExternalAPIClient AppM)

-- A monad that you use for testing
newtype TestAppM a = TestAppM {runTestAppM :: IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO
    )
  deriving (ExternalAPI) via (MockedExternalAPI TestAppM)