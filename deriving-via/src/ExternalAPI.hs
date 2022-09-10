module ExternalAPI
  ( ExternalAPI (..),
    ExternalThingId,
    ExternalThing (..),
    MockedExternalAPI (..),
    RealExternalAPIClient (..),
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))

-- Imagine that we have some complicated functionality that we want to
-- put behind a typeclass, such as an integration with some external API.
class ExternalAPI m where
  getExternal :: ExternalThingId -> m ExternalThing
  postExternal :: ExternalThing -> m ExternalThingId

-- These could be any external data types
type ExternalThingId = Int

data ExternalThing = ExternalThing

-- We want to provide different implementation for that API, e.g.
-- one mocked one for testing, and one real one for production.

-- Mocked implementation
newtype MockedExternalAPI m a = MockedExternalAPI (m a)

instance (MonadIO m) => ExternalAPI (MockedExternalAPI m) where
  getExternal externalId = MockedExternalAPI $ do
    liftIO $ putStrLn $ "Called mocked getExternal with id " <> show externalId <> "..."
    pure ExternalThing
  postExternal _ = MockedExternalAPI $ do
    liftIO $ putStrLn "Called mocked postExternal..."
    pure 1

-- "Real" implementation
newtype RealExternalAPIClient m a = RealExternalAPIClient (m a)

instance (MonadIO m) => ExternalAPI (RealExternalAPIClient m) where
  getExternal externalId = RealExternalAPIClient $
    liftIO $ do
      putStrLn $ "Calling real API with id " <> show externalId <> "..."
      threadDelay $ 1000 * 500 -- Half a second
      pure ExternalThing

  postExternal _ = RealExternalAPIClient $
    liftIO $ do
      putStrLn "Posting to the real API..."
      threadDelay $ 1000 * 500
      pure 123
