module Bank.TrueLayer.DataAPI.Accounts
  ( accounts
  ) where

import Bank.TrueLayer.Internal
  ( Endpoint
  , AccessToken
  , getWithAuthAndOptions
  , defaults
  , header
  , fromString
  , (&)
  , (.~)
  )
import Bank.TrueLayer.DataAPI.Schema
  ( Accounts
  )


accounts :: String -> Endpoint -> AccessToken -> IO (Maybe Accounts)
accounts ip = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions defaults "/data/v1/accounts"
