module Bank.TrueLayer.Internal
  ( getWithAuth
  , getWithAuthAndOptions
  , postWithAuth
  , defaults
  , header
  , param
  , fromString
  , (.~)
  , (&)
  , (</>)
  , Options
  , Endpoint(..)
  , AccessToken(..)
  ) where

import           Control.Lens         ((&), (.~), (?~), (^.))
import           Data.Aeson           (FromJSON, ToJSON (..))
import           Data.String          (fromString)
import           Data.Text.Encoding   (encodeUtf8)
import           Network.OAuth.OAuth2 (AccessToken (..))
import           Network.Wreq
    (Options, asJSON, auth, defaults, getWith, header, oauth2Bearer, param, postWith, responseBody)
import           System.FilePath      ((</>))

newtype Endpoint = Endpoint String

getWithAuth :: FromJSON a => String -> Endpoint -> AccessToken -> IO (Maybe a)
getWithAuth uri (Endpoint endpoint) (AccessToken accessToken) = do
  let opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 accessToken)
  r <- asJSON =<< getWith opts (endpoint ++ uri)
  return (r ^. responseBody)

getWithAuthAndOptions :: FromJSON a => Options -> String -> Endpoint -> AccessToken -> IO (Maybe a)
getWithAuthAndOptions options uri (Endpoint endpoint) (AccessToken accessToken) = do
  let opts = options & auth ?~ oauth2Bearer (encodeUtf8 accessToken)
  r <- asJSON =<< getWith opts (endpoint ++ uri)
  return (r ^. responseBody)

postWithAuth :: (ToJSON body, FromJSON res) => String -> body -> Endpoint -> AccessToken -> IO (Maybe res)
postWithAuth uri body (Endpoint endpoint) (AccessToken accessToken) = do
  let opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 accessToken)
  r <- asJSON =<< postWith opts (endpoint ++ uri) (toJSON body)
  return (r ^. responseBody)
