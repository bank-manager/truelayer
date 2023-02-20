module Bank.TrueLayer.DataAPI.Accounts
  ( accounts
  , account
  , accountBalance
  , transactions
  , pendingTransactions
  , standingOrders
  , directDebits
  ) where

import           Bank.TrueLayer.DataAPI.Schema
    ( AccountBalances
    , AccountId (..)
    , Accounts
    , DirectDebits
    , Ip (..)
    , StandingOrders
    , TransactionParams
    , Transactions
    , addTransactionParams
    )
import           Bank.TrueLayer.Internal
    (AccessToken, Endpoint, defaults, fromString, getWithAuthAndOptions, header, (&), (.~), (</>))


accounts :: Ip -> Endpoint -> AccessToken -> IO (Maybe Accounts)
accounts (Ip ip) = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions opts "/data/v1/accounts"

account :: Ip -> AccountId -> Endpoint -> AccessToken -> IO (Maybe Accounts)
account (Ip ip) (AccountId accountId) = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions opts ("/data/v1/accounts" </> accountId)

accountBalance :: Ip -> AccountId -> Endpoint -> AccessToken -> IO (Maybe AccountBalances)
accountBalance (Ip ip) (AccountId accountId) = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions opts ("/data/v1/accounts" </> accountId </> "balance")

transactions :: Ip -> AccountId -> Maybe TransactionParams -> Endpoint -> AccessToken -> IO (Maybe Transactions)
transactions (Ip ip) (AccountId accountId) params endpoint accessToken = do
  opts <- addTransactionParams params (defaults & header "X-PSU-IP" .~ [fromString ip])
  getWithAuthAndOptions opts ("/data/v1/accounts" </> accountId </> "transactions")endpoint accessToken

pendingTransactions :: Ip -> AccountId -> Maybe TransactionParams -> Endpoint -> AccessToken -> IO (Maybe Transactions)
pendingTransactions (Ip ip) (AccountId accountId) params endpoint accessToken = do
  opts <- addTransactionParams params (defaults & header "X-PSU-IP" .~ [fromString ip])
  getWithAuthAndOptions opts ("/data/v1/accounts" </> accountId </> "transactions/pending") endpoint accessToken

standingOrders :: Ip -> AccountId -> Endpoint -> AccessToken -> IO (Maybe StandingOrders)
standingOrders (Ip ip) (AccountId accountId) = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions opts ("/data/v1/accounts" </> accountId </> "standing-orders")

directDebits :: Ip -> AccountId -> Endpoint -> AccessToken -> IO (Maybe DirectDebits)
directDebits (Ip ip) (AccountId accountId) = do
  let opts = defaults & header "X-PSU-IP" .~ [fromString ip]
  getWithAuthAndOptions opts ("/data/v1/accounts" </> accountId </> "direct-debits")
