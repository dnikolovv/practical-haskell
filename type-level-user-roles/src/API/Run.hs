{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module API.Run (run) where

import API.Definition (API, server)
import Data.String (IsString (..))
import Data.Text (Text)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Application, Context (EmptyContext, (:.)), Proxy (Proxy), serveWithContext)
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings, fromSecret)
import qualified System.IO as IO

run :: IO ()
run =
  runSettings
    ( setPort port $
        setBeforeMainLoop
          (IO.putStrLn $ "Running on port " <> show port <> "...")
          defaultSettings
    )
    . logStdoutDev
    $ application
  where
    port = 3003

application :: Application
application = do
  let jwtCfg = defaultJWTSettings (fromSecret . fromString $ "this secret is kept very very securely")
      cookieCfg = defaultCookieSettings
      context = cookieCfg :. jwtCfg :. EmptyContext

  serveWithContext (Proxy @API) context server

-- For local testing purposes
_testAdminToken :: Text
_testAdminToken = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJ0ZXN0IiwiaWF0IjoxNjYyNjM4MTgxLCJleHAiOjIwNzI4NjUzODEsImF1ZCI6Ind3dy5leGFtcGxlLmNvbSIsInN1YiI6ImFkbWluIiwidXNlcm5hbWUiOiJ0ZXN0IiwidXNlclR5cGUiOiJBZG1pblVzZXJUeXBlIn0.028zC1mVIy7NStHAKqN8rVAW27YoS6pJA7B_fqVMLgA"

_testRegularToken :: Text
_testRegularToken = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJ0ZXN0IiwiaWF0IjoxNjYyNjM4MTgxLCJleHAiOjIwNzI4NjUzODEsImF1ZCI6Ind3dy5leGFtcGxlLmNvbSIsInN1YiI6ImFkbWluIiwidXNlcm5hbWUiOiJ0ZXN0IiwidXNlclR5cGUiOiJSZWd1bGFyVXNlclR5cGUifQ.3vLEDw80urs7pRMEhOasrQ7qi0x_oHQ6LF_QlZCdilY"