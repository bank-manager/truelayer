module Bank.TrueLayer.Internal
  ( getWithAuth
  , getWithAuthAndOptions
  , postWithAuth
  , defaults
  , header
  , fromString
  , (.~)
  , (&)
  , (</>)
  , Options
  , Endpoint(..)
  , AccessToken(..)
  ) where

import Control.Lens
  ( (&)
  , (^.)
  , (?~)
  , (.~)
  )
import Data.Aeson (FromJSON, ToJSON(..))
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import Network.Wreq
  ( asJSON
  , getWith
  , postWith
  , oauth2Bearer
  , defaults
  , auth
  , responseBody
  , Options
  , header
  )
import Network.OAuth.OAuth2 (AccessToken(..))
import System.FilePath ((</>))

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
