{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Bank.TrueLayer.Auth
  ( genAccessToken
  ) where

import Network.OAuth.OAuth2 (OAuth2(..), RefreshToken, ExchangeToken, OAuth2Token, refreshAccessToken)
import URI.ByteString.QQ (uri)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

oauthSettings :: OAuth2
oauthSettings = OAuth2
  { oauthClientId = "bankmanager-772883"
  , oauthClientSecret = Just ""
  , oauthOAuthorizeEndpoint = [uri|https://auth.truelayer.com/connect/token|]
  , oauthAccessTokenEndpoint = [uri|https://auth.truelayer.com/connect/token|]
  , oauthCallback = Just [uri|https://console.truelayer.com/redirect-page|]
  }

genAccessToken :: RefreshToken -> IO (Maybe OAuth2Token)
genAccessToken refreshToken = do
  manager <- newManager tlsManagerSettings
  token <- refreshAccessToken manager oauthSettings refreshToken
  return (
    case token of
      Left err -> Nothing
      Right token -> Just token
    )
