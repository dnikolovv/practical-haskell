{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module API.Definition (API, server) where

import API.WithAuth
import Data.Text
import Servant
import Servant.Auth.Server

type API =
  PublicAPI
    :<|> (Auth '[JWT] AnyAPIUser :> ProtectedAPI)

type PublicAPI =
  "public" :> Get '[JSON] Text

type ProtectedAPI =
  RegularAPI :<|> AdminAPI

type RegularAPI =
  "regular" :> Get '[JSON] Text

type AdminAPI =
  "admin" :> Get '[JSON] Text

server :: Server API
server =
  publicServer
    :<|> protectedServer
  where
    publicServer :: Handler Text
    publicServer = anonymousUserHandler

    protectedServer :: AuthResult AnyAPIUser -> Server ProtectedAPI
    protectedServer (Authenticated user) = do
      regularServer user
        :<|> adminServer user
    protectedServer _ =
      throwAll err401

    adminServer :: AnyAPIUser -> Server AdminAPI
    adminServer (AnyAPIUser user) = case user of
      AdminUser _ -> hoistServer (Proxy @AdminAPI) (runAsAdmin user) adminOnlyHandler
      _ -> throwAll err401

    regularServer :: AnyAPIUser -> Server RegularAPI
    regularServer (AnyAPIUser user) = case user of
      AdminUser _ -> hoistServer (Proxy @RegularAPI) (runAdminToRegular user) regularHandler
      RegularUser _ -> hoistServer (Proxy @RegularAPI) (runAsRegularUser user) regularHandler

anonymousUserHandler :: Handler Text
anonymousUserHandler =
  pure "you hit the anonymous handler!"

type AsRegularUser a = forall uts. WithAuth ('RegularUserType : uts) Handler a

regularHandler :: AsRegularUser Text
regularHandler = protected $ do
  pure "you hit the regular handler!"

type AsAdmin a = forall uts. WithAuth ('AdminUserType : uts) Handler a

adminOnlyHandler :: AsAdmin Text
adminOnlyHandler = protected $ do
  pure "you hit the admin only handler!"