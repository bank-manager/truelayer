module Bank.TrueLayer.DataAPI.Accounts
  ( accounts
  ) where

import           Bank.TrueLayer.DataAPI.Schema (Accounts)
import           Bank.TrueLayer.Internal
    (AccessToken, Endpoint, defaults, fromString, getWithAuthAndOptions, header, (&), (.~))


accounts :: String -> Endpoint -> AccessToken -> IO (Maybe Accounts)
accounts ip = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions defaults "/data/v1/accounts"
