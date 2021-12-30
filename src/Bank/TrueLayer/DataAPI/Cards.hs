module Bank.TrueLayer.DataAPI.Cards
  ( cards
  , cardBalance
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
  , (</>)
  )

import Bank.TrueLayer.DataAPI.Schema
  ( Cards
  , CardBalances
  , AccountId(..)
  )

cards :: String -> Endpoint -> AccessToken -> IO (Maybe Cards)
cards ip = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions defaults "/data/v1/cards"


cardBalance :: String -> AccountId -> Endpoint -> AccessToken -> IO (Maybe CardBalances)
cardBalance ip (AccountId accountId) = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions defaults ("/data/v1/cards" </> accountId </> "balance")
