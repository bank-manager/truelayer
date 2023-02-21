{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Bank.TrueLayer.Auth
  ( genAccessToken
  , swapCode
  , buildOAuth2
  , getAuthorizationUrl
  , RefreshToken(..)
  , AccessToken(..)
  , OAuth2Token(..)
  , OAuth2
  , ExchangeToken
  , Env(..)
  ) where

import           Data.Bifunctor                 ( bimap )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Network.HTTP.Client            ( newManager )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Network.OAuth.OAuth2           ( AccessToken(..)
                                                , ExchangeToken
                                                , OAuth2(..)
                                                , OAuth2Token(..)
                                                , RefreshToken(..)
                                                , appendQueryParams
                                                , authorizationUrl
                                                , fetchAccessToken
                                                , refreshAccessToken
                                                )
import           URI.ByteString                 ( URI )
import           URI.ByteString.QQ              ( uri )


newtype ClientId = ClientId Text deriving Show
newtype ClientSecret = ClientSecret Text deriving Show

data Env = Sandbox | Prod


buildOAuth2 :: Env -> ClientId -> ClientSecret -> URI -> OAuth2
buildOAuth2 env (ClientId clientId) (ClientSecret clientSecret) callback =
  OAuth2 { oauthClientId            = clientId
         , oauthClientSecret        = Just clientSecret
         , oauthOAuthorizeEndpoint  = getAuthorizeEndpoint env
         , oauthAccessTokenEndpoint = getAccessTokenEndpoint env
         , oauthCallback            = Just callback
         }


getAuthorizeEndpoint :: Env -> URI
getAuthorizeEndpoint Prod    = [uri|https://auth.truelayer.com|]
getAuthorizeEndpoint Sandbox = [uri|https://auth-sandbox.truelayer.com|]


getAccessTokenEndpoint :: Env -> URI
getAccessTokenEndpoint Prod = [uri|https://auth.truelayer.com/connect/token|]
getAccessTokenEndpoint Sandbox =
  [uri|https://auth-sandbox.truelayer.com/connect/token|]


getAuthorizationUrl :: OAuth2 -> [(Text, Text)] -> URI
getAuthorizationUrl oauth2Settings params = appendQueryParams
  bytestringParams
  (authorizationUrl oauth2Settings)
  where bytestringParams = map (bimap encodeUtf8 encodeUtf8) params


genAccessToken :: OAuth2 -> RefreshToken -> IO (Maybe OAuth2Token)
genAccessToken oauthSettings token = do
  manager <- newManager tlsManagerSettings
  eToken  <- refreshAccessToken manager oauthSettings token
  return $ case eToken of
    Left  _ -> Nothing
    Right t -> Just t


swapCode :: OAuth2 -> ExchangeToken -> IO (Maybe OAuth2Token)
swapCode oauthSettings code = do
  manager <- newManager tlsManagerSettings
  eToken  <- fetchAccessToken manager oauthSettings code
  return $ case eToken of
    Left  _     -> Nothing
    Right token -> Just token
