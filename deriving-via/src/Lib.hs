{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib (demo) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import ExternalAPI
  ( ExternalAPI (..),
    ExternalThing (..),
    -- Note that you need to import the constructors as well
    MockedExternalAPI (..),
    RealExternalAPIClient (..),
  )

-- *Lib> demo
-- Calling real API with id 123...
-- Posting to the real API...
-- Called mocked getExternal with id 123...
-- Called mocked postExternal...
-- *Lib> 

demo :: IO ()
demo = do
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