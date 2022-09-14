{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API.WithAuth
  ( WithAuth,
    UserType (..),
    UserData (..),
    AnyAPIUser (..),
    APIUser (..),
    runAsAdmin,
    runAsRegularUser,
    runAdminToRegular,
    protected,
  )
where

import Control.Arrow (first)
import Crypto.JWT (emptyClaimsSet, unregisteredClaims)
import Data.Aeson (FromJSON, Result (..), ToJSON (toJSON), Value (Object), fromJSON)
import Data.Aeson.Key (fromText, toText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Lens.Micro (set, to, (^.))
import RIO.Map qualified as Map
import Servant.Auth.JWT

data UserType
  = AdminUserType
  | RegularUserType
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

data UserData = UserData
  { username :: Text,
    userType :: UserType
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

data AnyAPIUser = forall ut. AnyAPIUser (APIUser ut)

instance ToJWT AnyAPIUser where
  encodeJWT (AnyAPIUser user) = do
    case toJSON (userData user) of
      (Object userAsMap) ->
        set
          unregisteredClaims
          ( Map.fromList
              . map (first toText)
              . KeyMap.toList
              $ userAsMap
          )
          emptyClaimsSet
      _ -> emptyClaimsSet

instance FromJWT AnyAPIUser where
  decodeJWT m =
    case fromJSON
      ( m
          ^. unregisteredClaims
          . to
            ( Object
                . KeyMap.fromList
                . map (first fromText)
                . Map.toList
            )
      ) of
      Success ud@UserData {..} -> case userType of
        AdminUserType -> Right . AnyAPIUser $ AdminUser ud
        RegularUserType -> Right . AnyAPIUser $ RegularUser ud
      Error err -> Left (T.pack err)

data APIUser (ut :: UserType) where
  AdminUser :: UserData -> APIUser 'AdminUserType
  RegularUser :: UserData -> APIUser 'RegularUserType

userData :: APIUser ut -> UserData
userData = \case
  AdminUser u -> u
  RegularUser u -> u

newtype WithAuth (uts :: [UserType]) m a = WithAuth (m a)

protected ::
  forall uts m a.
  m a ->
  WithAuth uts m a
protected = WithAuth

runAsRegularUser ::
  APIUser 'RegularUserType ->
  WithAuth ('RegularUserType : uts) m a ->
  m a
runAsRegularUser _ = coerce

runAdminToRegular ::
  APIUser 'AdminUserType ->
  WithAuth ('RegularUserType : uts) m a ->
  m a
runAdminToRegular _ = coerce

runAsAdmin ::
  APIUser 'AdminUserType ->
  WithAuth ('AdminUserType : uts) m a ->
  m a
runAsAdmin _ = coerce