module Bank.TrueLayer.DataAPI.Cards
  ( cards
  , cardBalance
  ) where

import           Bank.TrueLayer.Internal
    (AccessToken, Endpoint, defaults, fromString, getWithAuthAndOptions, header, (&), (.~), (</>))

import           Bank.TrueLayer.DataAPI.Schema (AccountId (..), CardBalances, Cards)

cards :: String -> Endpoint -> AccessToken -> IO (Maybe Cards)
cards ip = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions defaults "/data/v1/cards"


cardBalance :: String -> AccountId -> Endpoint -> AccessToken -> IO (Maybe CardBalances)
cardBalance ip (AccountId accountId) = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions defaults ("/data/v1/cards" </> accountId </> "balance")
