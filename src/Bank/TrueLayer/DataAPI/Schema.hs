{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Bank.TrueLayer.DataAPI.Schema where

import           Data.Aeson              (FromJSON (..), ToJSON (..))
import           Data.Time.Calendar      (Day)
import           Data.Time.LocalTime     (ZonedTime)
import           GHC.Generics            (Generic)

import           Bank.TrueLayer.Internal (Options, fromString, param, (&), (.~))

newtype Ip = Ip String
  deriving (Show)



newtype Accounts = Accounts { results :: [Account] }
  deriving (FromJSON, Generic, Show, ToJSON)

data Account = Account
                 { update_timestamp :: !ZonedTime
                 , account_id       :: !(Maybe AccountId)
                 , account_type     :: !(Maybe String)
                 , display_name     :: !(Maybe String)
                 , currency         :: !(Maybe String)
                 , accountNumber    :: !AccountNumber
                 , provider         :: !Provider
                 }
  deriving (FromJSON, Generic, Show, ToJSON)

data AccountBalance = AccountBalance
                        { currency         :: !String
                        , available        :: !Double
                        , current          :: !Double
                        , overdraft        :: !(Maybe Double)
                        , update_timestamp :: !(Maybe String)
                        }
  deriving (FromJSON, Generic, Show, ToJSON)

data AccountBalances = AccountBalances
                         { results :: ![AccountBalance]
                         , status  :: !(Maybe String)
                         }
  deriving (FromJSON, Generic, Show, ToJSON)


newtype AccountId = AccountId String
  deriving (FromJSON, Generic, Show, ToJSON)

data AccountNumber = AccountNumber
                       { number         :: !(Maybe String)
                       , sort_code      :: !(Maybe String)
                       , swift_bic      :: !(Maybe String)
                       , iban           :: !(Maybe String)
                       , routing_number :: !(Maybe String)
                       }
  deriving (FromJSON, Generic, Show, ToJSON)

data Card = Card
              { account_id          :: !AccountId
              , card_network        :: !String
              , card_type           :: !String
              , currency            :: !String
              , display_name        :: !String
              , partial_card_number :: !String
              , name_on_card        :: !String
              , valid_from          :: !(Maybe String)
              , valid_to            :: !(Maybe String)
              , update_timestamp    :: !ZonedTime
              , provider            :: !Provider
              }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype Cards = Cards { results :: [Card] }
  deriving (FromJSON, Generic, Show, ToJSON)


newtype CardBalances = CardBalances { results :: [CardBalance] }
  deriving (FromJSON, Generic, Show, ToJSON)

data CardBalance = CardBalance
                     { available              :: !Double
                     , currency               :: !String
                     , current                :: !Double
                     , credit_limit           :: !Double
                     , last_statement_balance :: !(Maybe Double)
                     , last_statement_date    :: !(Maybe Day)
                     , payment_due            :: !(Maybe Double)
                     , payment_due_date       :: !(Maybe Day)
                     , update_timestamp       :: !ZonedTime
                     }
  deriving (FromJSON, Generic, Show, ToJSON)

data DirectDebit = DirectDebit
                     { direct_debit_id            :: !String
                     , timestamp                  :: !String
                     , name                       :: !String
                     , status                     :: !String
                     , previous_payment_timestamp :: !(Maybe String)
                     , previous_payment_amount    :: !(Maybe Double)
                     , currency                   :: !(Maybe String)
                     , meta                       :: !(Maybe DirectDebitMeta)
                     }
  deriving (FromJSON, Generic, Show, ToJSON)

data DirectDebitMeta = DirectDebitMeta
                         { provider_mandate_identification :: !(Maybe String)
                         , provider_account_id             :: !(Maybe String)
                         }
  deriving (FromJSON, Generic, Show, ToJSON)

data DirectDebits = DirectDebits
                      { results :: ![DirectDebit]
                      , status  :: !String
                      }
  deriving (FromJSON, Generic, Show, ToJSON)


newtype Provider = Provider { provider_id :: ProviderId }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype ProviderId = ProviderId String
  deriving (FromJSON, Generic, Show, ToJSON)

data RunningBalance = RunningBalance
                        { amoung   :: !Double
                        , currency :: !String
                        }
  deriving (FromJSON, Generic, Show, ToJSON)

data StandingOrder = StandingOrder
                       { frequency            :: !String
                       , status               :: !(Maybe String)
                       , timestamp            :: !String
                       , currency             :: !(Maybe String)
                       , meta                 :: !(Maybe StandingOrderMeta)
                       , next_payment_date    :: !(Maybe String)
                       , next_payment_amount  :: !(Maybe Double)
                       , first_payment_date   :: !(Maybe String)
                       , first_payment_amount :: !(Maybe Double)
                       , final_payment_date   :: !(Maybe String)
                       , final_payment_amount :: !(Maybe Double)
                       , payee                :: !(Maybe String)
                       , reference            :: !(Maybe String)
                       }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype StandingOrderMeta = StandingOrderMeta { provider_account_id :: Maybe String }
  deriving (FromJSON, Generic, Show, ToJSON)

data StandingOrders = StandingOrders
                        { results :: ![StandingOrder]
                        , status  :: !String
                        }
  deriving (FromJSON, Generic, Show, ToJSON)

data Transaction = Transaction
                     { transaction_id                     :: !TransactionId
                     , normalised_provider_transaction_id :: !(Maybe TransactionId)
                     , provider_transaction_id            :: !(Maybe TransactionId)
                     , timestamp                          :: !ZonedTime
                     , description                        :: !String
                     , amount                             :: !Double
                       -- pounds
                     , currency                           :: !String
                     , transaction_type                   :: !String
                     , transaction_category               :: !String
                     , transaction_classification         :: ![String]
                     , merchant_name                      :: !(Maybe String)
                     , running_balance                    :: !(Maybe RunningBalance)
                     , meta                               :: !(Maybe TransactionMeta)
                     }
  deriving (FromJSON, Generic, Show, ToJSON)

data TransactionMeta = TransactionMeta
                         { provider_transaction_category :: !(Maybe String)
                         , provider_reference            :: !(Maybe String)
                         , provider_merchant_name        :: !(Maybe String)
                         , provider_category             :: !(Maybe String)
                         , address                       :: !(Maybe String)
                         , provider_id                   :: !(Maybe ProviderId)
                         , counter_party_preferred_name  :: !(Maybe String)
                         , counter_party_iban            :: !(Maybe String)
                         , user_comments                 :: !(Maybe String)
                         , debtor_account_name           :: !(Maybe String)
                         , transaction_type              :: !(Maybe String)
                         , provider_source               :: !(Maybe String)
                         , cardNumber                    :: !(Maybe String)
                         , location                      :: !(Maybe String)
                         }
  deriving (FromJSON, Generic, Show, ToJSON)

newtype TransactionId = TransactionId String
  deriving (FromJSON, Generic, Show, ToJSON)

newtype Transactions = Transactions { results :: [Transaction] }
  deriving (FromJSON, Generic, Show, ToJSON)

data TransactionParams = TransactionParams
                           { to   :: !(Maybe String)
                           , from :: !(Maybe String)
                           }
  deriving (Show)

addTransactionParams :: Maybe TransactionParams -> Options -> IO Options
addTransactionParams (Just TransactionParams { from = mFrom, to = mTo }) opts =
    do
        opts' <- case mTo of
            Just to -> return $ opts & param "to" .~ [fromString to]
            Nothing -> return opts
        case mFrom of
            Just from -> return $ opts' & param "from" .~ [fromString from]
            Nothing   -> return opts'
addTransactionParams Nothing opts = return opts
