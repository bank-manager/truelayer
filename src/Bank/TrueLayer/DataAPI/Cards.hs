module Bank.TrueLayer.DataAPI.Cards
  ( cards
  , card
  , cardBalance
  , transactions
  , pendingTransactions
  ) where

import           Bank.TrueLayer.Internal
    ( AccessToken
    , Endpoint
    , defaults
    , fromString
    , getWithAuthAndOptions
    , header
    , (&)
    , (.~)
    , (</>)
    )

import           Bank.TrueLayer.DataAPI.Schema
    ( AccountId (..)
    , CardBalances
    , Cards
    , Ip (..)
    , TransactionParams
    , Transactions
    , addTransactionParams
    )

cards :: Ip -> Endpoint -> AccessToken -> IO (Maybe Cards)
cards (Ip ip) = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions opts "/data/v1/cards"

card :: Ip -> AccountId -> Endpoint -> AccessToken -> IO (Maybe Cards)
card (Ip ip) (AccountId accountId) = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions opts ("/data/v1/cards" </> accountId)

cardBalance :: Ip -> AccountId -> Endpoint -> AccessToken -> IO (Maybe CardBalances)
cardBalance (Ip ip) (AccountId accountId) = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions opts ("/data/v1/cards" </> accountId </> "balance")

transactions :: Ip -> AccountId -> Maybe TransactionParams -> Endpoint -> AccessToken -> IO (Maybe Transactions)
transactions (Ip ip) (AccountId accountId) params endpoint accessToken = do
  opts <- addTransactionParams params (defaults & header "X-PSU-IP" .~ [fromString ip])
  getWithAuthAndOptions opts ("/data/v1/cards" </> accountId </> "transactions") endpoint accessToken

pendingTransactions :: Ip -> AccountId -> Maybe TransactionParams -> Endpoint -> AccessToken -> IO (Maybe Transactions)
pendingTransactions (Ip ip) (AccountId accountId) params endpoint accessToken = do
  opts <- addTransactionParams params (defaults & header "X-PSU-IP" .~ [fromString ip])
  getWithAuthAndOptions opts ("/data/v1/cards" </> accountId </> "transactions/pending") endpoint accessToken
