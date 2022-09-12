module API.Playing where

import Control.Monad.Except (ExceptT, MonadTrans (lift), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Data.Text
import Servant
import Prelude hiding (log)

type Note = Text

data MailgunEmailBody

data ProcessingFailure = InvalidNote
  deriving (Show)

data ValidationFailure = ValidationFailure
  deriving (Show)

class HasMailgunSigningKey a where

class HasLog a where

type Processor = ExceptT ProcessingFailure IO

log :: (MonadReader env m, MonadIO m, HasLog env) => String -> m ()
log = undefined

makeNoteFromBody :: MailgunEmailBody -> IO Note
makeNoteFromBody = undefined

validateMessage :: (MonadReader e m, MonadIO m, HasMailgunSigningKey e) => MailgunEmailBody -> m (Either ValidationFailure Bool)
validateMessage = undefined

toHandler :: Processor a -> Handler a
toHandler = undefined

data State

type AppM a = ReaderT State Handler a

mailgunMessageHandler ::
  (HasLog e, HasMailgunSigningKey e) =>
  (Note -> Processor ()) ->
  MailgunEmailBody ->
  ReaderT e Handler ()
mailgunMessageHandler validateNote b = do
  valid <- validateMessage b
  note <- liftIO $ makeNoteFromBody b
  
  case valid of
    Left err -> do
      log $ "Rejected message: " <> show err
      lift . toHandler $ throwError InvalidNote
    Right _ -> do
      handleResult <- liftIO . runExceptT $ validateNote note

      case handleResult of
        Left err -> do
          log $ "Error when handling: " <> show err
          lift . toHandler $ throwError InvalidNote
        Right _ -> pure ()