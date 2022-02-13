{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Bank.TrueLayer.Auth
  ( genAccessToken
  , swapCode
  ) where

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.OAuth.OAuth2
    (ExchangeToken, OAuth2 (..), OAuth2Token, RefreshToken, fetchAccessToken, refreshAccessToken)
import           URI.ByteString.QQ       (uri)

oauthSettings :: OAuth2
oauthSettings = OAuth2
  { oauth2ClientId = "bankmanager-772883"
  , oauth2ClientSecret = Just ""
  , oauth2AuthorizeEndpoint = [uri|https://auth.truelayer.com/connect/token|]
  , oauth2TokenEndpoint = [uri|https://auth.truelayer.com/connect/token|]
  , oauth2RedirectUri = Just [uri|https://console.truelayer.com/redirect-page|]
  }

genAccessToken :: RefreshToken -> IO (Maybe OAuth2Token)
genAccessToken refreshToken = do
  manager <- newManager tlsManagerSettings
  token <- refreshAccessToken manager oauthSettings refreshToken
  return $ case token of
    Left err    -> Nothing
    Right token -> Just token


swapCode :: ExchangeToken -> IO (Maybe OAuth2Token)
swapCode code = do
  manager <- newManager tlsManagerSettings
  token <- fetchAccessToken manager  oauthSettings code
  return $ case token of
    Left err    -> Nothing
    Right token -> Just token
